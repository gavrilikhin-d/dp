use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{
    alts,
    bootstrap::rules::AtomicPattern,
    errors::Error,
    obj,
    parsers::{ParseResult, Parser},
    rule, rule_ref, seq, Context,
};

use super::Pattern;

/// Repeat pattern
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
pub struct Repeat {
    /// Pattern to repeat
    pub pattern: Box<Pattern>,
    /// Minimum number of repetitions
    #[serde(default)]
    pub at_least: usize,
    /// Maximum number of repetitions
    pub at_most: Option<usize>,
}
rule!(
    Repeat:
        {
            alts!(
                seq!(
                    {pattern: AtomicPattern} '*'
                    => obj!(Repeat { pattern })
                ),
                seq!(
                    {pattern: AtomicPattern} '+'
                    => obj!(Repeat { pattern, at_least: 1 })
                ),
                seq!(
                    {pattern: AtomicPattern} '?'
                    => obj!(Repeat { pattern, at_most: 1 })
                ),
                rule_ref!(AtomicPattern)
            )
        }
);

impl Repeat {
    /// Repeat pattern zero or more times (`*`)
    pub fn zero_or_more(pattern: impl Into<Pattern>) -> Self {
        Self {
            pattern: Box::new(pattern.into()),
            at_least: 0,
            at_most: None,
        }
    }

    /// Repeat pattern once or more times (`+`)
    pub fn once_or_more(pattern: impl Into<Pattern>) -> Self {
        Self {
            pattern: Box::new(pattern.into()),
            at_least: 1,
            at_most: None,
        }
    }

    /// Repeat pattern at most once (`?`)
    pub fn at_most_once(pattern: impl Into<Pattern>) -> Self {
        Self {
            pattern: Box::new(pattern.into()),
            at_least: 0,
            at_most: Some(1),
        }
    }

    /// Repeat pattern at least `at_least` times
    pub fn at_least(at_least: usize, pattern: impl Into<Pattern>) -> Self {
        Self {
            pattern: Box::new(pattern.into()),
            at_least,
            at_most: None,
        }
    }
}

impl Parser for Repeat {
    fn parse_at<'s>(
        &self,
        source: &'s str,
        at: usize,
        context: &mut Context,
    ) -> Result<ParseResult, Error> {
        debug_assert!(self.at_least <= self.at_most.unwrap_or(usize::MAX));

        let mut delta = 0;
        let mut asts = Vec::new();
        for _ in 0..self.at_least {
            let res = self.pattern.parse_at(source, at + delta, context)?;
            delta += res.delta;
            asts.push(res.ast);
        }

        for _ in self.at_least..self.at_most.unwrap_or(usize::MAX) {
            let res = self.pattern.parse_at(source, at + delta, context);
            if let Ok(res) = res {
                delta += res.delta;
                asts.push(res.ast);
            } else {
                break;
            }
        }

        Ok(ParseResult {
            delta,
            ast: if self.at_most == Some(1) {
                if asts.len() == 1 {
                    asts.into_iter().next().unwrap()
                } else {
                    json!(null)
                }
            } else {
                asts.into()
            },
        })
    }
}

#[cfg(test)]
mod test {
    use crate::UnderlyingRule;
    use pretty_assertions::assert_eq;
    use serde_json::json;

    use crate::{
        errors::Expected,
        parsers::{ParseResult, Parser},
        Context,
    };

    use super::Repeat;

    #[test]
    fn repeat() {
        let mut context = Context::default();
        let r = Repeat::rule();
        assert_eq!(r.parse("x", &mut context).unwrap().ast, json!("x"));
        assert_eq!(
            r.parse("x?", &mut context).unwrap().ast,
            json!({
                "Repeat": {
                    "pattern": "x",
                    "at_most": 1
                }
            })
        );
        assert_eq!(
            r.parse("x*", &mut context).unwrap().ast,
            json!({
                "Repeat": {
                    "pattern": "x"
                }
            })
        );
        assert_eq!(
            r.parse("x+?", &mut context).unwrap().ast,
            json!({
                "Repeat": {
                    "pattern": "x",
                    "at_least": 1
                }
            })
        );
    }

    #[test]
    fn at_most_once() {
        let mut context = Context::default();
        let pattern = Repeat::at_most_once("a");
        assert_eq!(
            pattern.parse("", &mut context).unwrap(),
            ParseResult::empty().with_ast(json!(null))
        );
        assert_eq!(
            pattern.parse("a", &mut context).unwrap(),
            ParseResult {
                delta: 1,
                ast: "a".into()
            }
        );
        assert_eq!(
            pattern.parse("aa", &mut context).unwrap(),
            ParseResult {
                delta: 1,
                ast: "a".into()
            }
        )
    }

    #[test]
    fn zero_or_more() {
        let mut context = Context::default();
        let pattern = Repeat::zero_or_more("a");
        assert_eq!(
            pattern.parse("", &mut context).unwrap(),
            ParseResult::empty().with_ast(json!([]))
        );
        assert_eq!(
            pattern.parse("a", &mut context).unwrap(),
            ParseResult {
                delta: 1,
                ast: vec!["a"].into()
            }
        );
        assert_eq!(
            pattern.parse("aa", &mut context).unwrap(),
            ParseResult {
                delta: 2,
                ast: json!(["a", "a"])
            }
        );
    }

    #[test]
    fn once_or_more() {
        let mut context = Context::default();
        let pattern = Repeat::once_or_more("a");
        assert_eq!(
            pattern.parse("", &mut context).unwrap_err(),
            Expected {
                expected: "a".to_string(),
                at: 0
            }
            .into()
        );
        assert_eq!(
            pattern.parse("a", &mut context).unwrap(),
            ParseResult {
                delta: 1,
                ast: vec!["a"].into()
            }
        );
        assert_eq!(
            pattern.parse("aa", &mut context).unwrap(),
            ParseResult {
                delta: 2,
                ast: json!(["a", "a"])
            }
        );
    }
}
