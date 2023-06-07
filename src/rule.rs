use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{
    bootstrap::rules::RuleName,
    errors::Error,
    parsers::{ParseResult, Parser},
    rule, source_id, Context, Key, Pattern,
};

/// Trait for types that may be converted to rule
pub trait UnderlyingRule {
    /// Get name of the rule
    fn name() -> &'static str;
    /// Get the underlying rule representation
    fn rule() -> Rule;
}

/// Syntax rule
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
pub struct Rule {
    /// Rule name
    pub name: String,
    /// Pattern to parse
    pub pattern: Pattern,
}
rule!(Rule: {name: RuleName} ':' {pattern: Pattern});

impl Rule {
    /// Create a new rule with a name and a pattern
    pub fn new(name: impl Into<String>, pattern: impl Into<Pattern>) -> Self {
        Self {
            name: name.into(),
            pattern: pattern.into(),
        }
    }

    /// Generate key to cache
    fn key<'s>(&self, source: &'s str, at: usize) -> Key {
        Key {
            source_id: source_id(source),
            at,
            id: self.name.clone(),
        }
    }
}

impl Parser for Rule {
    fn parse_at<'s>(
        &self,
        source: &'s str,
        at: usize,
        context: &mut Context,
    ) -> Result<ParseResult, Error> {
        if let Some(res) = context.fetch(&self.key(source, at)) {
            return res.clone();
        }

        let mut result = self.pattern.parse_at(source, at, context);
        if let Ok(ref mut res) = &mut result {
            // single unnamed -> transparent
            // has action -> transparent
            // named -> wrap
            // sequence with named without actions -> wrap
            if self.pattern.is_named()
                || self
                    .pattern
                    .as_sequence()
                    .is_some_and(|s| s.has_named() && s.action.is_none())
            {
                res.ast = json!({ &self.name: res.ast.take() });
            }

            if let Some(on_parsed) = context.on_parsed(&self.name) {
                res.ast = on_parsed(res.ast.take(), context);
            }
        }

        context.cache(self.key(source, at), result.clone());

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::{bootstrap::rules::Type, obj, patterns::Repeat, rule};

    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn single_unnamed_pattern_not_wrapped() {
        rule!(struct Test: r"/[^\s]+/");

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("Hello World", &mut context).unwrap(),
            ParseResult {
                delta: 5,
                ast: json!("Hello")
            }
        );
    }

    #[test]
    fn single_named_pattern_wrapped() {
        rule!(struct Test: {text: r"/[^\s]+/"});

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("Hello World", &mut context).unwrap(),
            ParseResult {
                delta: 5,
                ast: json!({"Test": {"text": "Hello"}})
            }
        );
    }

    #[test]
    fn single_named_pattern_with_action() {
        rule!(struct Test: {text: r"/[^\s]+/"} => text);

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("Hello World", &mut context).unwrap(),
            ParseResult {
                delta: 5,
                ast: json!("Hello")
            }
        );
    }

    #[test]
    fn sequence_without_named_ignored() {
        rule!(struct Test: 'a' 'b');

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("a b", &mut context).unwrap().ast,
            json!({})
        );
    }

    #[test]
    fn sequence_with_named_wrapped() {
        rule!(struct Test: 'a' {name: "b"});

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("a b", &mut context).unwrap().ast,
            json!({
			"Test": {"name": "b"}})
        );
    }

    #[test]
    fn test_deserialize_rule_with_sequence() {
        let mut context = Context::default();
        let rule = context.find_rule("Rule").unwrap();
        assert_eq!(rule.name, "Rule");

        assert_eq!(
            rule.parse("X: a b", &mut context).unwrap(),
            ParseResult {
                delta: 6,
                ast: json!({
                    "Rule": {
                        "name": "X",
                        "pattern": ["a", "b"]
                    }
                })
            }
        );

        let rule = context.find_rule("X").unwrap();
        assert_eq!(rule.name, "X");
        assert_eq!(rule.pattern, vec!["a".into(), "b".into()].into());
    }

    #[test]
    fn test_deserialize_rule_with_regex() {
        let mut context = Context::default();
        let rule = context.find_rule("Rule").unwrap();
        assert_eq!(rule.name, "Rule");

        assert_eq!(
            rule.parse("X: /ab?c/", &mut context).unwrap(),
            ParseResult {
                delta: 9,
                ast: json!({
                    "Rule": {
                        "name": "X",
                        "pattern": "/ab?c/"
                    }
                })
            }
        );

        let rule = context.find_rule("X").unwrap();
        assert_eq!(rule.name, "X");
        assert_eq!(rule.pattern, r"/ab?c/".into());
    }

    #[test]
    fn deserialize_and_parse_rule_with_action() {
        let mut context = Context::default();
        let rule = context.find_rule("Rule").unwrap();
        assert_eq!(rule.name, "Rule");

        assert_eq!(
            rule.parse("List: '(' <letters: x*> ')' => letters", &mut context)
                .unwrap()
                .ast,
            json!({
                "Rule": {
                    "name": "List",
                    "pattern": {
                        "Sequence": {
                            "patterns": [
                                '(',
                                {
                                    "Named": {
                                        "name": "letters",
                                        "pattern": {
                                            "Repeat": {
                                                "pattern": "x",
                                            }
                                        }
                                    }
                                },
                                ')'
                            ],
                            "action": {
                                "Return": {
                                    "Variable": "letters"
                                }
                            }
                        }
                    }
                }
            })
        );

        let rule = context.find_rule("List").unwrap();
        rule!(struct List: '(' {letters: Repeat::zero_or_more("x")} ')' => letters);
        assert_eq!(rule.as_ref(), &List::rule());
        assert_eq!(
            rule.parse("(x x)", &mut context).unwrap().ast,
            json!(["x", "x"])
        )
    }

    #[test]
    fn deserialize_and_parse_rule_with_throw() {
        let mut context = Context::default();
        let rule = context.find_rule("Rule").unwrap();
        assert_eq!(rule.name, "Rule");

        assert_eq!(
            rule.parse(
                "List: '(' =>
					throw CustomError {
						message: \"expected closing ')'\"
					}
				",
                &mut context
            )
            .unwrap()
            .ast,
            json!({
                "Rule": {
                    "name": "List",
                    "pattern": {
                        "Sequence": {
                            "patterns": ['('],
                            "action": {
                                "Throw": {
                                    "CustomError": {
                                        "message": "expected closing ')'"
                                    }
                                }
                            }
                        }
                    }
                }
            })
        );

        let rule = context.find_rule("List").unwrap();
        rule!(
            struct List:
            "(" => throw obj!(
                CustomError {
                    message: "expected closing ')'"
                }
            )
        );
        assert_eq!(rule.as_ref(), &List::rule());
        assert_eq!(rule.parse("(", &mut context).unwrap().ast, json!(null))
    }

    #[test]
    fn rule_action_with_variable_type() {
        let mut context = Context::default();
        let rule = context.find_rule("Rule").unwrap();
        assert_eq!(rule.name, "Rule");

        assert_eq!(
            rule.parse("X: <ty: Type> => {} as ty", &mut context)
                .unwrap()
                .ast,
            json!({
                "Rule": {
                    "name": "X",
                    "pattern": {
                        "Sequence": {
                            "patterns": [
                                {
                                    "Named": {
                                        "name": "ty",
                                        "pattern": {
                                            "RuleReference": "Type"
                                        }
                                    }
                                }
                            ],
                            "action": {
                                "Return": {
                                    "Cast": {
                                        "expr": {},
                                        "ty": {
                                            "Variable": "ty"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            })
        );

        let rule = context.find_rule("X").unwrap();
        rule!(
            struct X:
            {ty: Type} => (obj! {}) as ty
        );
        assert_eq!(rule.as_ref(), &X::rule(),);
        assert_eq!(
            rule.parse("Person", &mut context).unwrap().ast,
            json!({"Person": {}})
        )
    }
}
