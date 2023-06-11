use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

use colored::Colorize;
use log::{debug, info, trace};
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{
    bootstrap::rules::RuleName,
    errors::{CustomError, Error, Severity},
    parsers::{ParseResult, Parser},
    rule, source_id, Context, Key, Pattern,
};

/// Limit of recursion for rules
const RECURSION_LIMIT: usize = 32;

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

    /// Do something with call depth of this function
    fn using_call_depth<R>(&self, f: impl FnOnce(&mut usize) -> R) -> R {
        static DEPTH: LazyLock<Mutex<HashMap<String, usize>>> =
            LazyLock::new(|| Mutex::new(HashMap::new()));
        f(DEPTH.lock().unwrap().entry(self.name.clone()).or_default())
    }
}

impl Parser for Rule {
    fn parse_at<'s>(
        &self,
        source: &'s str,
        at: usize,
        context: &mut Context,
    ) -> Result<ParseResult, Error> {
        let target = format!("{}@{at}", self.name);

        let err = self.using_call_depth(|depth| {
            if *depth <= RECURSION_LIMIT {
                return None;
            }

            Some(CustomError {
                message: "Recursion limit reached".to_string(),
                severity: Severity::Error,
                code: None,
                help: None,
                labels: None,
                url: None,
            })
        });
        if let Some(err) = err {
            debug!(
                target: target.as_str(),
                "Recursion limit reached"
            );
            let result = Err(err.into());
            return result;
        }

        if let Some(res) = context.fetch(&self.key(source, at)) {
            trace!(
                target: target.as_str(),
                "Cache hit"
            );
            log_result(target.as_str(), &res);
            return res.clone();
        }

        trace!(target: target.as_ref(), "Parsing");

        self.using_call_depth(|depth| *depth += 1);
        let mut result = self.pattern.parse_at(source, at, context);
        log_result(target.as_str(), &result);
        self.using_call_depth(|depth| *depth -= 1);

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
                trace!(
                    target: target.as_str(),
                    "Calling {}", "on_parsed".bold()
                );
                res.ast = on_parsed(res.ast.take(), context);
            }
        }

        context.cache(self.key(source, at), result.clone());

        result
    }
}

fn log_result<T, E>(target: &str, result: &Result<T, E>) {
    let msg = if result.is_ok() {
        "OK".green().bold()
    } else {
        "ERR".red().bold()
    };
    debug!(
        target: target,
        "{msg}",
    );
}

#[cfg(test)]
mod tests {
    use crate::{alts, bootstrap::rules::Type, obj, patterns::Repeat, rule, seq};

    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn single_unnamed_pattern_not_wrapped() {
        rule!(struct Test: r"/[^\s]+/");

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("Hello World", &mut context).unwrap().ast,
            json!("Hello")
        );
    }

    #[test]
    fn single_named_pattern_wrapped() {
        rule!(struct Test: {text: r"/[^\s]+/"});

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("Hello World", &mut context).unwrap().ast,
            json!({"Test": {"text": "Hello"}})
        );
    }

    #[test]
    fn single_named_pattern_with_action() {
        rule!(struct Test: {text: r"/[^\s]+/"} => text);

        let mut context = Context::new();
        assert_eq!(
            Test::rule().parse("Hello World", &mut context).unwrap().ast,
            json!("Hello")
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
            rule.parse("X: a b", &mut context).unwrap().ast,
            json!({
                "Rule": {
                    "name": "X",
                    "pattern": ["a", "b"]
                }
            })
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
            rule.parse("X: /ab?c/", &mut context).unwrap().ast,
            json!({
                "Rule": {
                    "name": "X",
                    "pattern": "/ab?c/"
                }
            })
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

    #[test]
    fn left_recursion() {
        let mut context = Context::new();

        rule!(struct E: {
            alts!(
                seq!({lhs: E} '+' {rhs: E} => obj!(E { lhs, rhs })),
                "/[0-9]/"
            )
        });
        context.add_rule(E::rule().into());
        assert_eq!(
            E::rule().parse("1 + 2 + 3", &mut context).unwrap().ast,
            json!({
                "E": {
                    "lhs": {
                        "E": {
                            "lhs": "1",
                            "rhs": "2"
                        }
                    },
                    "rhs": "3"
                }
            })
        )
    }
}
