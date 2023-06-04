use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{
    bootstrap::rules::RuleName,
    parsers::{ParseResult, Parser},
    rule_ref, seq, Context, ParseTree, Pattern,
};

/// Syntax rule
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
pub struct Rule {
    /// Rule name
    pub name: String,
    /// Pattern to parse
    pub pattern: Pattern,
}

impl Rule {
    /// Create a new rule with a name and a pattern
    pub fn new(name: impl Into<String>, pattern: impl Into<Pattern>) -> Self {
        Self {
            name: name.into(),
            pattern: pattern.into(),
        }
    }

    pub fn rule() -> Rule {
        Rule::new(
            "Rule",
            seq!(
                ("name", rule_ref!(RuleName)),
                ":",
                ("pattern", rule_ref!(Pattern))
            ),
        )
    }
}

impl Parser for Rule {
    fn parse_at<'s>(&self, source: &'s str, at: usize, context: &mut Context) -> ParseResult<'s> {
        let mut res = self.pattern.parse_at(source, at, context);
        res.tree = ParseTree::named(self.name.clone()).with(res.tree).flatten();

        if res.has_errors() {
            return res;
        }

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
            on_parsed(at, res, context)
        } else {
            res
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        action::{reference, ret, throw},
        obj,
        patterns::Repeat,
        rule, rule_ref,
    };

    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn single_unnamed_pattern_not_wrapped() {
        rule!(Test: r"/[^\s]+/");

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("Hello World", &mut context),
            ParseResult {
                delta: 5,
                tree: ParseTree::named("Test").with("Hello"),
                ast: json!("Hello")
            }
        );
    }

    #[test]
    fn single_named_pattern_wrapped() {
        rule!(Test: ("text", r"/[^\s]+/"));

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("Hello World", &mut context),
            ParseResult {
                delta: 5,
                tree: ParseTree::named("Test").with("Hello"),
                ast: json!({"Test": {"text": "Hello"}})
            }
        );
    }

    #[test]
    fn single_named_pattern_with_action() {
        rule!(Test: seq!(("text", r"/[^\s]+/") => ret(reference("text"))));

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("Hello World", &mut context),
            ParseResult {
                delta: 5,
                tree: ParseTree::named("Test").with("Hello"),
                ast: json!("Hello")
            }
        );
    }

    #[test]
    fn sequence_without_named_ignored() {
        rule!(Test: seq!("a", "b"));

        let mut context = Context::new();
        assert_eq!(Test::rule().parse("a b", &mut context).ast, json!({}));
    }

    #[test]
    fn sequence_with_named_wrapped() {
        rule!(Test: seq!("a", ("name", "b")));

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("a b", &mut context).ast,
            json!({
			"Test": {"name": "b"}})
        );
    }

    #[test]
    fn test_deserialize_rule_with_sequence() {
        let mut context = Context::default();
        let rule = context.find_rule("Rule").unwrap();
        assert_eq!(rule.name, "Rule");

        let tree_text = json!({
            "Rule": [
                { "RuleName": "X" },
                ":",
                {
                    "Pattern": {
                        "Alternatives": {
                            "Sequence": [
                                {
                                    "Repeat": {
                                        "AtomicPattern": {
                                            "Text": {
                                                "trivia": " ",
                                                "value": "a",
                                            }
                                        }
                                    }
                                },
                                {
                                    "Repeat": {
                                        "AtomicPattern": {
                                            "Text": {
                                                "trivia": " ",
                                                "value": "b",
                                            }
                                        }
                                    }
                                }
                            ]
                        }
                    }
                }
            ]
        })
        .to_string();
        assert_eq!(
            rule.parse("X: a b", &mut context),
            ParseResult {
                delta: 6,
                tree: serde_json::from_str(&tree_text).unwrap(),
                ast: json!(
                    {
                        "name": "X",
                        "pattern": ["a", "b"]
                    }
                )
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

        let tree_text = json!({
            "Rule": [
                { "RuleName": "X" },
                ":",
                {
                    "Pattern": {
                        "Alternatives": {
                            "Sequence": [
                                {
                                    "Repeat": {
                                        "AtomicPattern": {
                                            "Regex": {
                                                "trivia": " ",
                                                "value": "/ab?c/",
                                            }
                                        }
                                    }
                                },
                            ]
                        }
                    }
                }
            ]
        })
        .to_string();
        assert_eq!(
            rule.parse("X: /ab?c/", &mut context),
            ParseResult {
                delta: 9,
                tree: serde_json::from_str(&tree_text).unwrap(),
                ast: json!(
                    {
                        "name": "X",
                        "pattern": "/ab?c/"
                    }
                )
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
                .ast,
            json!({
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
            })
        );

        let rule = context.find_rule("List").unwrap();
        assert_eq!(
            rule.as_ref(),
            &Rule::new(
                "List",
                seq!(
                    '(',
                    ("letters", Repeat::zero_or_more("x")),
                    ')'
                     =>
                    ret(reference("letters"))
                )
            )
        );
        assert_eq!(rule.parse("(x x)", &mut context).ast, json!(["x", "x"]))
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
            .ast,
            json!({
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
            })
        );

        let rule = context.find_rule("List").unwrap();
        assert_eq!(
            rule.as_ref(),
            &Rule::new(
                "List",
                seq!(
                    '(' => throw(
                        obj!(CustomError { message: "expected closing ')'" })
                    )
                )
            )
        );
        assert_eq!(rule.parse("(", &mut context).ast, json!(null))
    }

    #[test]
    fn rule_action_with_variable_type() {
        let mut context = Context::default();
        let rule = context.find_rule("Rule").unwrap();
        assert_eq!(rule.name, "Rule");

        assert_eq!(
            rule.parse("X: <ty: Type> => {} as ty", &mut context).ast,
            json!({
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
            })
        );

        let rule = context.find_rule("X").unwrap();
        assert_eq!(
            rule.as_ref(),
            &Rule::new(
                "X",
                seq!(
                    ("ty", rule_ref!("Type")) =>
                    ret(obj! {}.cast_to(reference("ty")))
                )
            )
        );
        assert_eq!(
            rule.parse("Person", &mut context).ast,
            json!({"Person": {}})
        )
    }
}
