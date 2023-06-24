use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    sync::Arc,
};

use serde_json::json;

use crate::{
    action::Action,
    bootstrap::rules::{
        Alternatives, AtomicPattern, Char, Comment, Distinct, DistinctObject, DistinctValue,
        Expand, Identifier, Integer, NonEmptyObject, Object, Regex, Return, Root, RuleName,
        RuleReference, Text, Throw, Type, Typename, Value, Variable,
    },
    expressions::{
        ArrayConstructor, ArrayElement, Cast, FieldInitializer, Initializer, ObjectConstructor,
    },
    parser,
    patterns::{Named, Repeat, Sequence},
    Expression, Pattern, Rule, UnderlyingRule,
};

/// Action to be executed after parsing
pub type OnParsedAction =
    for<'c> fn(ast: serde_json::Value, context: &'c mut Context) -> serde_json::Value;

/// Helper function to make a rule transparent and remove quotes
fn without_quotes(ast: serde_json::Value, _: &mut Context) -> serde_json::Value {
    let s = ast.as_str().unwrap();
    json!(s[1..s.len() - 1])
}

/// Rule with action to be executed after parsing
#[derive(Debug)]
pub struct RuleWithAction {
    pub rule: Arc<Rule>,
    pub on_parsed: Option<OnParsedAction>,
}

impl RuleWithAction {
    /// Create a new rule with an action
    pub fn new(rule: Rule, on_parsed: OnParsedAction) -> Self {
        RuleWithAction {
            rule: Arc::new(rule),
            on_parsed: Some(on_parsed),
        }
    }
}

impl From<Rule> for RuleWithAction {
    fn from(rule: Rule) -> Self {
        RuleWithAction {
            rule: Arc::new(rule),
            on_parsed: None,
        }
    }
}

/// Keys inside cache
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Key {
    /// Id of source
    pub source_id: u64,
    /// Position at which parsed
    pub at: usize,
    /// Rule's name or pattern's id
    pub id: String,
}
pub type Cache = HashMap<Key, parser::ParseResult>;

/// Parsing context
#[derive(Debug)]
pub struct Context {
    /// Parsing rules
    pub rules: HashMap<String, RuleWithAction>,
    /// Cached rules
    pub cache: Cache,
}

impl Context {
    /// Create a new context without any rules
    pub fn new() -> Context {
        Context {
            rules: HashMap::new(),
            cache: Cache::new(),
        }
    }

    // Add a rule to the context
    pub fn add_rule(&mut self, rule: RuleWithAction) -> Option<RuleWithAction> {
        self.rules.insert(rule.rule.name.clone(), rule)
    }

    /// Find rule by name in the parsing context
    pub fn find_rule(&self, name: &str) -> Option<Arc<Rule>> {
        self.rules.get(name).map(|r| r.rule.clone())
    }

    /// Get the callback to be called after parsing a rule
    pub fn on_parsed(&self, name: &str) -> Option<OnParsedAction> {
        self.rules.get(name).map(|r| r.on_parsed).flatten()
    }

    /// Cache parsing result at given position
    pub fn cache(&mut self, key: Key, result: parser::ParseResult) {
        self.cache.insert(key, result);
    }

    /// Fetch parsing result at given position from cache
    pub fn fetch(&self, key: &Key) -> Option<&parser::ParseResult> {
        self.cache.get(key)
    }
}

impl Default for Context {
    fn default() -> Self {
        let mut ctx = Context::new();
        let rules = vec![
            Root::rule().into(),
            RuleWithAction::new(Char::rule(), without_quotes),
            RuleWithAction::new(Integer::rule(), |ast, _| {
                let str = ast.as_str().unwrap();
                if let Ok(i) = str.parse::<i64>() {
                    return i.into();
                }

                json!({ "Integer": str })
            }),
            RuleWithAction::new(String::rule(), without_quotes),
            Text::rule().into(),
            Regex::rule().into(),
            RuleName::rule().into(),
            RuleReference::rule().into(),
            Pattern::rule().into(),
            RuleWithAction::new(Alternatives::rule(), |mut ast, _| {
                let alts = ast.as_array_mut().unwrap();
                let mut alts = alts.iter().map(|a| a.get("x").unwrap());
                if alts.len() == 1 {
                    return json!(alts.next().unwrap().clone());
                }

                json!({ "Alternatives": alts.collect::<Vec<_>>() })
            }),
            Action::rule().into(),
            Return::rule().into(),
            Throw::rule().into(),
            Sequence::rule().into(),
            Repeat::rule().into(),
            AtomicPattern::rule().into(),
            Named::rule().into(),
            Identifier::rule().into(),
            NonEmptyObject::rule().into(),
            Object::rule().into(),
            ObjectConstructor::rule().into(),
            Type::rule().into(),
            Typename::rule().into(),
            Cast::rule().into(),
            Expression::rule().into(),
            Value::rule().into(),
            FieldInitializer::rule().into(),
            RuleWithAction::new(Rule::rule(), |ast, context| {
                let rule: Rule = serde_json::from_value(ast.get("Rule").unwrap().clone()).unwrap();
                context.add_rule(rule.into());
                ast
            }),
            Variable::rule().into(),
            DistinctObject::rule().into(),
            DistinctValue::rule().into(),
            Distinct::rule().into(),
            Expand::rule().into(),
            Initializer::rule().into(),
            ArrayElement::rule().into(),
            ArrayConstructor::rule().into(),
            Comment::rule().into(),
        ];
        rules.into_iter().for_each(|r| {
            ctx.add_rule(r);
        });
        ctx
    }
}

/// Get source id
pub fn source_id(source: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    source.hash(&mut hasher);
    hasher.finish()
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use pretty_assertions::assert_eq;

    use serde_json::json;

    use crate::{
        bootstrap::rules::Root, errors::Expected, parser::Parser, rule, source_id, syntax::Node,
        Context, Key, Rule, UnderlyingRule,
    };

    #[test]
    fn typename() {
        let mut context = Context::default();
        let r = context.find_rule("Type").unwrap();
        assert_eq!(r.name, "Type");
        assert_eq!(r.parse("Type", &mut context).ast.unwrap(), json!("Type"));
    }

    #[test]
    fn cast() {
        let mut context = Context::default();
        let r = context.find_rule("Cast").unwrap();
        assert_eq!(r.name, "Cast");
        assert_eq!(
            r.parse("{ name: \"Igor\" } as Person", &mut context)
                .ast
                .unwrap(),
            json!({"Cast": {"expr": { "name": "Igor" }, "ty": "Person"}})
        );
    }

    #[test]
    fn action() {
        let mut context = Context::default();
        let r = context.find_rule("Action").unwrap();
        assert_eq!(r.name, "Action");
        assert_eq!(
            r.parse("=> 'x'", &mut context).ast.unwrap(),
            json!({"Return": 'x'})
        );
        assert_eq!(
            r.parse("=> throw x", &mut context).ast.unwrap(),
            json!({
                "Throw": {
                        "Variable": "x"
                }
            })
        );
    }

    #[test]
    fn throw() {
        let mut context = Context::default();
        let r = context.find_rule("Throw").unwrap();
        assert_eq!(r.name, "Throw");
        assert_eq!(
            r.parse("throw 'x'", &mut context).ast.unwrap(),
            json!({"Throw": 'x'})
        );
        assert_eq!(
            r.parse("throw { message: \"msg\"} as CustomError", &mut context)
                .ast
                .unwrap(),
            json!({
                "Throw": {
                    "Cast": {
                        "expr": {
                            "message": "msg"
                        },
                        "ty": "CustomError"
                    }
                }
            })
        )
    }

    #[test]
    fn ret() {
        let mut context = Context::default();
        let r = context.find_rule("Return").unwrap();
        assert_eq!(r.name, "Return");
        assert_eq!(
            r.parse("'x'", &mut context).ast.unwrap(),
            json!({"Return": 'x'})
        );
    }

    #[test]
    fn object() {
        let mut ctx = Context::default();
        let r = ctx.find_rule("Object").unwrap();
        assert_eq!(r.name, "Object");
        assert_eq!(r.parse("{}", &mut ctx).ast.unwrap(), json!({}));
        assert_eq!(
            r.parse("{x: \"x\"}", &mut ctx).ast.unwrap(),
            json!({"x": "x"})
        );
        assert_eq!(
            r.parse("{x: 'x', y: {},}", &mut ctx).ast.unwrap(),
            json!({'x': 'x', 'y': {}})
        );
    }

    #[test]
    fn expression() {
        let mut ctx = Context::default();
        let r = ctx.find_rule("Expression").unwrap();
        assert_eq!(r.name, "Expression");
        assert_eq!(r.parse("{}", &mut ctx).ast.unwrap(), json!({}));
        assert_eq!(
            r.parse("{} as Person", &mut ctx).ast.unwrap(),
            json!({"Cast": { "expr": {}, "ty": "Person" }})
        );
        assert_eq!(r.parse("'('", &mut ctx).ast.unwrap(), json!('('));
        assert_eq!(r.parse("\"()\"", &mut ctx).ast.unwrap(), json!("()"));
        assert_eq!(
            r.parse("x", &mut ctx).ast.unwrap(),
            json!({ "Variable": "x" })
        );
        assert_eq!(r.parse("123", &mut ctx).ast.unwrap(), json!(123));
    }

    #[test]
    fn integer() {
        let mut ctx = Context::default();
        let r = ctx.find_rule("Integer").unwrap();
        assert_eq!(r.name, "Integer");
        assert_eq!(r.parse("123", &mut ctx).ast.unwrap(), json!(123));

        let big_integer = "99999999999999999999999999999999";
        assert_eq!(
            r.parse(big_integer, &mut ctx).ast.unwrap(),
            json!({ "Integer": big_integer })
        );
    }

    #[test]
    fn variable() {
        let mut ctx = Context::default();
        let r = ctx.find_rule("Variable").unwrap();
        assert_eq!(r.name, "Variable");
        assert_eq!(
            r.parse("x", &mut ctx).ast.unwrap(),
            json!({ "Variable": "x" })
        );
    }

    #[test]
    fn field_initializer() {
        let mut ctx = Context::default();
        let r = ctx.find_rule("FieldInitializer").unwrap();
        assert_eq!(r.name, "FieldInitializer");
        assert_eq!(r.parse("x: 'x'", &mut ctx).ast.unwrap(), json!({"x": "x"}));
    }

    #[test]
    fn char() {
        let mut ctx = Context::default();
        let rule_name = ctx.find_rule("Char").unwrap();
        assert_eq!(rule_name.name, "Char");
        assert_eq!(rule_name.parse("'x'", &mut ctx).ast.unwrap(), json!("x"));
    }

    #[test]
    fn string() {
        let mut ctx = Context::default();
        let rule_name = ctx.find_rule("String").unwrap();
        assert_eq!(rule_name.name, "String");
        assert_eq!(
            rule_name.parse("\"abc\"", &mut ctx).ast.unwrap(),
            json!("abc")
        );
    }

    #[test]
    fn rule_name() {
        let mut ctx = Context::default();
        let rule_name = ctx.find_rule("RuleName").unwrap();
        assert_eq!(rule_name.name, "RuleName");
        assert_eq!(rule_name.parse("Foo", &mut ctx).ast.unwrap(), json!("Foo"));
        assert_eq!(
            rule_name.parse("foo", &mut ctx).syntax,
            vec![
                crate::syntax::Node::from(0..3).with_name("name"),
                crate::errors::RuleNameNotCapitalized { at: 0..3 }.into()
            ]
            .into()
        );
        assert_eq!(
            rule_name.parse("", &mut ctx).syntax,
            Node::from(Expected {
                expected: "[a-z_][a-zA-Z0-9_]*".into(),
                at: 0
            })
            .with_name("name")
        );
    }

    #[test]
    fn identifier() {
        let mut ctx = Context::default();
        let r = ctx.find_rule("Identifier").unwrap();
        assert_eq!(r.name, "Identifier");
        assert_eq!(r.parse("foo", &mut ctx).ast.unwrap(), json!("foo"));
    }

    #[test]
    fn named_pattern() {
        let mut ctx = Context::default();
        let r = ctx.find_rule("Named").unwrap();
        assert_eq!(r.name, "Named");

        assert_eq!(
            r.parse("<name: /[a-z]+/>", &mut ctx).ast.unwrap(),
            json!({"Named": { "name": "name", "pattern": "/[a-z]+/" }})
        );
    }

    #[test]
    fn rule_reference() {
        let mut context = Context::default();
        let r = context.find_rule("RuleReference").unwrap();
        assert_eq!(r.name, "RuleReference");
        assert_eq!(
            r.parse("Foo", &mut context).ast.unwrap(),
            json!({"RuleReference": "Foo"})
        );
    }
    #[test]
    fn atomic_pattern() {
        let mut context = Context::default();
        let r = context.find_rule("AtomicPattern").unwrap();
        assert_eq!(r.name, "AtomicPattern");

        assert_eq!(
            r.parse("Foo", &mut context).ast.unwrap(),
            json!({
                "RuleReference": "Foo"
            })
        );

        assert_eq!(r.parse("foo", &mut context).ast.unwrap(), json!("foo"));

        assert_eq!(
            r.parse("/(xyz?)/", &mut context).ast.unwrap(),
            json!("/(xyz?)/")
        );

        assert_eq!(r.parse("( bar )", &mut context).ast.unwrap(), json!("bar"));
    }

    #[test]
    fn sequence() {
        let mut context = Context::default();
        let r = context.find_rule("Sequence").unwrap();
        assert_eq!(r.name, "Sequence");

        assert_eq!(
            r.parse("Foo bar?", &mut context).ast.unwrap(),
            json!([
            {
                "RuleReference": "Foo"
            },
            {
                "Repeat": {
                    "pattern": "bar",
                    "at_most": 1
                }
            }])
        );
        assert_eq!(
            r.parse("'(' <l: /[a-z]/> ')' => l", &mut context)
                .ast
                .unwrap(),
            json!({"Sequence": {
                "patterns": [
                    '(',
                    {
                        "Named": {
                            "name": "l",
                            "pattern": "/[a-z]/"
                        }
                    },
                    ')'
                ],
                "action": {
                    "Return": {
                        "Variable": "l"
                    }
                }
            }})
        )
    }

    #[test]
    fn alternatives() {
        let mut context = Context::default();
        let r = context.find_rule("Alternatives").unwrap();
        assert_eq!(r.name, "Alternatives");

        assert_eq!(
            r.parse("a | b", &mut context).ast.unwrap(),
            json!({
                "Alternatives": [
                    "a",
                    "b"
                ]
            })
        );

        assert_eq!(r.parse("a", &mut context).ast.unwrap(), json!("a"));

        assert_eq!(
            r.parse("a b | c d | e", &mut context).ast.unwrap(),
            json!({"Alternatives": [
                ["a", "b"],
                ["c", "d"],
                "e"
            ]})
        )
    }

    #[test]
    fn pattern() {
        let mut context = Context::default();
        let r = context.find_rule("Pattern").unwrap();
        assert_eq!(r.name, "Pattern");

        assert_eq!(
            r.parse("Foo?", &mut context).ast.unwrap(),
            json!({
                "Repeat": {
                    "pattern": {
                        "RuleReference": "Foo"
                    },
                    "at_most": 1
                }
            })
        );

        assert_eq!(
            r.parse("foo*", &mut context).ast.unwrap(),
            json!({
                "Repeat": {
                    "pattern": "foo"
                }
            })
        );

        assert_eq!(
            r.parse("(bar)+", &mut context).ast.unwrap(),
            json!({
                "Repeat": {
                    "pattern": "bar",
                    "at_least": 1
                }
            })
        );
    }

    #[test]
    fn rule() {
        let mut context = Context::default();
        let r = context.find_rule("Rule").unwrap();
        assert_eq!(r.name, "Rule");

        assert_eq!(
            r.parse("Lol: kek", &mut context).ast.unwrap(),
            json!({
                "Rule": {
                    "name": "Lol",
                    "pattern": "kek"
                }
            })
        );
        assert_eq!(
            context.find_rule("Lol"),
            Some(Arc::new(Rule {
                name: "Lol".to_string(),
                pattern: "kek".into()
            }))
        )
    }

    #[test]
    fn root() {
        let context = Context::default();

        assert_eq!(context.find_rule("Root"), Some(Arc::new(Root::rule())));
    }

    #[test]
    fn caching() {
        let mut context = Context::new();
        assert!(context.cache.is_empty());

        rule!(struct Test: 'a');
        let r = Test::rule();
        let res = r.parse("a", &mut context);
        assert_eq!(res.ast, Some(json!('a')));
        assert_eq!(
            &res,
            context
                .fetch(&Key {
                    source_id: source_id("a"),
                    at: 0,
                    id: "Test".to_string()
                })
                .unwrap()
        );

        // Even after changing rule we should still get same result
        let r = Rule::new("Test", "b");
        let res = r.parse("a", &mut context);
        assert_eq!(
            &res,
            context
                .fetch(&Key {
                    source_id: source_id("a"),
                    at: 0,
                    id: "Test".to_string()
                })
                .unwrap()
        )
    }
}
