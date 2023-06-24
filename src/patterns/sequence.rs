use std::ops::Range;

use derive_more::From;
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{
    action::Action,
    alts, obj,
    parser::{ParseOk, ParseResult, Parser},
    rule, rule_ref, seq, syntax, Context, Pattern,
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
        alts!(
            seq!(
                {patterns: Repeat::once_or_more(rule_ref!(Repeat))}
                {action: Action}
                => obj! (Sequence { patterns, action })
            ),
            Repeat::at_least(2, rule_ref!(Repeat)),
            rule_ref!(Repeat)
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

impl From<Vec<Pattern>> for Sequence {
    fn from(patterns: Vec<Pattern>) -> Self {
        Self {
            patterns,
            action: None,
        }
    }
}

impl Parser for Sequence {
    fn parse_at<'s>(&self, source: &'s str, mut at: usize, context: &mut Context) -> ParseResult {
        let mut syntax = vec![];
        let mut ast = json!({});
        for pattern in &self.patterns {
            let mut result = pattern.parse_at(source, at, context)?;
            at = result.syntax.range().map_or(at, |r| r.end);
            syntax.push(result.syntax);

            if let Pattern::Named(_) = pattern {
                ast.as_object_mut()
                    .unwrap()
                    .append(&mut result.ast.as_object_mut().unwrap());
            } else if self.patterns.len() == 1 {
                ast = result.ast;
                break;
            }
        }

        if let Some(action) = &self.action {
            let mut variables = ast.as_object().cloned().unwrap_or(serde_json::Map::new());
            variables.insert("@".to_string(), at.into());
            for s in syntax.iter() {
                if let syntax::Node::Named { name, node } = &s {
                    if let Some(r) = node.range() {
                        variables.insert(format!("@{}", name), json!(r));
                    }
                }
            }
            let result = action.execute(&variables);
            if let Err(error) = result {
                syntax.push(error.into());
                ast = json!(null);
            } else {
                ast = result.unwrap();
            }
        }

        Ok(ParseOk {
            syntax: syntax.into(),
            ast: ast.into(),
        })
    }
}

#[test]
fn sequence() {
    let mut context = Context::default();
    let r = Sequence::rule();
    assert_eq!(r.parse("x", &mut context).unwrap().ast, json!("x"));
    assert_eq!(r.parse("x y", &mut context).unwrap().ast, json!(["x", "y"]));
    assert_eq!(
        r.parse("x y => 1", &mut context).unwrap().ast,
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

    use crate::{alts, obj, parser::Parser, seq, Context};

    use super::Sequence;
    use pretty_assertions::assert_eq;

    #[test]
    fn named() {
        let mut context = Context::default();
        let p: Sequence = vec!['('.into(), ("pattern", "/[A-z][a-z]*/").into(), ')'.into()].into();

        assert_eq!(
            p.parse("( x )", &mut context).unwrap().ast,
            json!({"pattern": "x"})
        );
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

        assert_eq!(p.parse("( x )", &mut context).unwrap().ast, json!("x"));
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

        assert_eq!(p.parse("( x )", &mut context).unwrap().ast, json!({}));

        assert_eq!(p.parse("( x", &mut context).unwrap().ast, json!(null));
    }
}
