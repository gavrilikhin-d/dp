use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{
    action::Action,
    parsers::{ParseResult, Parser},
    rule, rule_ref, seq, Context, ParseTree, Pattern,
};

#[cfg(test)]
use crate::UnderlyingRule;
#[cfg(test)]
use pretty_assertions::assert_eq;

use super::Repeat;

/// Sequence of patterns with optional action
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
pub struct Sequence {
    /// Patterns to parse one after another
    pub patterns: Vec<Pattern>,
    /// Action to perform after parsing
    #[serde(default)]
    pub action: Option<Action>,
}
rule!(
    Sequence:
    {
        transparent(
            seq!(
                {patterns: Repeat::once_or_more(rule_ref!(Repeat))}
                {action: Repeat::at_most_once(rule_ref!(Action))}
            )
        )
    }
);

impl Sequence {
    /// Create a new sequence with an action
    pub fn new(patterns: Vec<Pattern>, action: Action) -> Self {
        Self {
            patterns,
            action: Some(action),
        }
    }

    /// Is there any named patterns in this sequence?
    pub fn has_named(&self) -> bool {
        self.patterns.iter().any(|p| p.is_named())
    }
}

/// Returns sequence like this: <x: Pattern> => x
pub fn transparent(pattern: impl Into<Pattern>) -> Sequence {
    let pattern = pattern.into();
    seq!(
        {x: pattern}
        => x
    )
}

impl From<Vec<Pattern>> for Sequence {
    fn from(patterns: Vec<Pattern>) -> Self {
        Self {
            patterns,
            action: None,
        }
    }
}

impl Parser for Sequence {
    fn parse_at<'s>(
        &self,
        source: &'s str,
        at: usize,
        context: &mut Context,
    ) -> crate::parsers::ParseResult<'s> {
        let mut delta = 0;
        let mut tree = ParseTree::empty();
        let mut ast = json!({});
        for pattern in &self.patterns {
            let mut result = pattern.parse_at(source, at + delta, context);
            let has_errors = result.has_errors();
            delta += result.delta;
            tree.push(result.tree);

            if let Pattern::Named(_) = pattern {
                ast.as_object_mut()
                    .unwrap()
                    .append(&mut result.ast.as_object_mut().unwrap());
            } else if self.patterns.len() == 1 {
                ast = result.ast;
                break;
            }

            if has_errors {
                return ParseResult {
                    delta,
                    tree: tree.flatten(),
                    ast: json!(null),
                };
            }
        }

        if let Some(action) = &self.action {
            let result = action.execute(&ast.as_object().unwrap_or(&serde_json::Map::new()));
            if let Err(error) = result {
                println!("{:?}", miette::Report::new(error));
                delta = 0;
                ast = json!(null);
            } else {
                ast = result.unwrap();
            }
        }

        ParseResult {
            delta,
            tree: tree.flatten(),
            ast: ast.into(),
        }
    }
}

#[test]
fn sequence() {
    let mut context = Context::default();
    let r = Sequence::rule();
    assert_eq!(r.parse("x", &mut context).ast, json!("x"));
    assert_eq!(r.parse("x y", &mut context).ast, json!(["x", "y"]));
    assert_eq!(
        r.parse("x y => 1", &mut context).ast,
        json!({
            "Sequence": {
                "patterns": ["x", "y"],
                "action": {
                    "Return": 1
                }
            }
        })
    );
}

#[cfg(test)]
mod test {
    use serde_json::json;

    use crate::{alts, obj, parsers::Parser, seq, Context};

    use super::Sequence;
    use pretty_assertions::assert_eq;

    #[test]
    fn named() {
        let mut context = Context::default();
        let p: Sequence = vec!['('.into(), ("pattern", "/[A-z][a-z]*/").into(), ')'.into()].into();

        assert_eq!(p.parse("( x )", &mut context).ast, json!({"pattern": "x"}));
    }

    #[test]
    fn action() {
        let mut context = Context::default();
        let p = seq!(
            '('
            {pattern: "/[A-z][a-z]*/"}
            ')'
            => pattern
        );

        assert_eq!(p.parse("( x )", &mut context).ast, json!("x"));
    }

    #[test]
    fn error() {
        let mut context = Context::default();
        let p = alts!(
            seq!('(' "/[A-z][a-z]*/" ')'),
            seq!(
                '(' "/[A-z][a-z]*/"
                => throw obj!(
                    Expected {
                        expected: ')',
                        at: 3
                    }
                )
            )
        );

        assert_eq!(p.parse("( x )", &mut context).ast, json!({}));

        assert_eq!(p.parse("( x", &mut context).ast, json!(null));
    }
}
