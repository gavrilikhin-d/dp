#[cfg(test)]
use crate::{parser::Parser, syntax::token::Kind, Context, UnderlyingRule};
#[cfg(test)]
use pretty_assertions::assert_eq;
#[cfg(test)]
use serde_json::json;

use crate::{
    action::merge,
    alts, cast,
    errors::RuleNameNotCapitalized,
    expr,
    expressions::{ArrayConstructor, Initializer, ObjectConstructor},
    obj,
    patterns::{separated, Named, Repeat, Sequence},
    rule, rule_ref, seq,
    syntax::Node,
    Expression, Pattern, Rule,
};

// ====================================
// IMPORTANT!!!
// DON'T FORGET TO ADD RULE TO CONTEXT!
// ====================================

rule!(
    struct Root:
        "/^/" {stmts: Repeat::zero_or_more(rule_ref!(Statement))} "/$/" => stmts
);
#[test]
fn root() {
    let mut context = Context::default();
    let r = Root::rule();
    rule!(struct X: "x");
    rule!(struct Y: "y");
    assert_eq!(
        r.parse("X: x; Y: y", &mut context).ast.unwrap(),
        json!([{"Rule": X::rule()}, {"Rule": Y::rule()}])
    );
}

rule!(struct Whitespace: {alts!(r"/\s+/", rule_ref!(Comment))});
#[test]
fn whitespace() {
    let mut context = Context::new();
    let r = Whitespace::rule();
    assert_eq!(r.parse(" ", &mut context).ast.unwrap(), json!(" "));
    assert_eq!(r.parse(" \t ", &mut context).ast.unwrap(), json!(" \t "));
}

rule!(struct Statement: Rule);
#[test]
fn statement() {
    let mut context = Context::default();
    let r = Statement::rule();
    rule!(struct R: "x");
    assert_eq!(
        r.parse("R: x", &mut context).ast.unwrap(),
        json!({"Rule": R::rule()})
    );
}

rule!(struct Comment: "////" {text: r"/[^\n]*/"} => cast!(Comment(text)));
#[test]
fn comment() {
    let mut context = Context::default();
    let r = Comment::rule();
    assert_eq!(
        r.parse("// text", &mut context).ast.unwrap(),
        json!({"Comment": " text"})
    );
}

rule!(struct Char: r"/'.'/");
#[test]
fn char() {
    let mut context = Context::default();
    let r = Char::rule();
    assert_eq!(r.parse("'c'", &mut context).ast.unwrap(), json!('c'));
}

rule!(struct Integer: r"/[0-9]+/");
#[test]
fn integer() {
    let mut context = Context::default();
    let r = Integer::rule();
    assert_eq!(r.parse("123", &mut context).ast.unwrap(), json!(123));

    let big_integer = "99999999999999999999999999999999";
    assert_eq!(
        r.parse(big_integer, &mut context).ast.unwrap(),
        json!({ "Integer": big_integer })
    );
}

rule!(String: "/\"([^\"\\\\]|\\.)*\"/");
#[test]
fn string() {
    let mut context = Context::default();
    let r = String::rule();
    assert_eq!(r.parse("\"str\"", &mut context).ast.unwrap(), json!("str"));
}

rule!(
    struct Text: {
        alts!(rule_ref!(Char), rule_ref!(String), r"/[^\s*+?()|<:>{}=;]+/")
    }
);
#[test]
fn text() {
    let mut context = Context::default();
    let r = Text::rule();
    assert_eq!(r.parse("'c'", &mut context).ast.unwrap(), json!('c'));
    assert_eq!(r.parse("\"str\"", &mut context).ast.unwrap(), json!("str"));
    assert_eq!(r.parse("text", &mut context).ast.unwrap(), json!("text"));
}

rule!(struct Regex: r"//[^/]+//");
#[test]
fn regex() {
    let mut context = Context::default();
    let r = Regex::rule();
    assert_eq!(r.parse("/ax?/", &mut context).ast.unwrap(), json!("/ax?/"));
}

rule!(
    struct RuleName: {
        alts!(
        r"/[A-Z][a-zA-Z0-9]*/",
        seq!(
            {name: r"/[a-z_][a-zA-Z0-9_]*/"}
            => throw obj!(RuleNameNotCapitalized { at: expr!(@name) })
        ))
    }
);
#[test]
fn rule_name() {
    let mut context = Context::default();
    let r = RuleName::rule();
    assert_eq!(r.parse("Rule", &mut context).ast.unwrap(), json!("Rule"));

    let res = r.parse("rule", &mut context);
    assert_eq!(res.ast, Some(json!(null)));
    assert_eq!(
        res.syntax,
        Node::named(
            "RuleName",
            vec![
                // FIXME: Kind::Type
                Node::from((Kind::Keyword, 0..4)).with_name("name"),
                RuleNameNotCapitalized { at: 0..4 }.into()
            ]
        )
    )
}

rule!(
    struct RuleReference: {name: r"/[A-Z][a-zA-Z0-9]*/"} => cast!(RuleReference(name))
);
#[test]
fn rule_reference() {
    let mut context = Context::default();
    let r = RuleReference::rule();
    assert_eq!(
        r.parse("RuleName", &mut context).ast.unwrap(),
        json!({"RuleReference": "RuleName"})
    );
}

rule!(struct Identifier: r"/[a-z_][a-zA-Z0-9_]*/");
#[test]
fn identifier() {
    let mut context = Context::default();
    let r = Identifier::rule();
    assert_eq!(r.parse("name", &mut context).ast.unwrap(), json!("name"));
}

rule!(struct Typename: "/[A-Z][a-zA-Z_0-9]*/");
#[test]
fn typename() {
    let mut context = Context::default();
    let r = Typename::rule();
    assert_eq!(r.parse("Type", &mut context).ast.unwrap(), json!("Type"));
}

rule!(struct Variable: {name: "/[@a-z_][a-zA-Z_0-9]*/"} => cast!(Variable(name)));
#[test]
fn variable() {
    let mut context = Context::default();
    let r = Variable::rule();
    assert_eq!(
        r.parse("var", &mut context).ast.unwrap(),
        json!({"Variable": "var"})
    );
    assert_eq!(
        r.parse("@", &mut context).ast.unwrap(),
        json!({"Variable": "@"})
    );
    assert_eq!(
        r.parse("@var", &mut context).ast.unwrap(),
        json!({"Variable": "@var"})
    );
}

rule!(struct Type: { alts!(Typename | Variable) });
#[test]
fn ty() {
    let mut context = Context::default();
    let r = Type::rule();
    assert_eq!(r.parse("Type", &mut context).ast.unwrap(), json!("Type"));
    assert_eq!(
        r.parse("var", &mut context).ast.unwrap(),
        json!({"Variable": "var"})
    );
}

rule!(struct Boolean: {alts!("true", "false")});
#[test]
fn boolean() {
    let mut context = Context::default();
    let r = Boolean::rule();
    assert_eq!(r.parse("true", &mut context).ast.unwrap(), json!(true));
    assert_eq!(r.parse("false", &mut context).ast.unwrap(), json!(false));
}

rule!(
    struct Value: {
        alts!(
            DistinctValue |
            Char |
            String |
            Boolean |
            Integer |
            ObjectConstructor |
            ArrayConstructor
        )
    }
);
#[test]
fn value() {
    let mut context = Context::default();
    let r = Value::rule();
    assert_eq!(r.parse("'c'", &mut context).ast.unwrap(), json!('c'));
    assert_eq!(r.parse("\"str\"", &mut context).ast.unwrap(), json!("str"));
    assert_eq!(r.parse("123", &mut context).ast.unwrap(), json!(123));
    assert_eq!(r.parse("{}", &mut context).ast.unwrap(), json!({}));
    assert_eq!(r.parse("[]", &mut context).ast.unwrap(), json!([]));
}

rule!(
    struct Object: {
        alts!(
            seq!('{' '}'),
            rule_ref!(NonEmptyObject)
        )
    }
);
#[test]
fn object() {
    let mut context = Context::default();
    let r = Object::rule();
    assert_eq!(r.parse("{}", &mut context).ast.unwrap(), json!({}));
    assert_eq!(
        r.parse("{a: 1, b: 2}", &mut context).ast.unwrap(),
        json!({"a": 1, "b": 2})
    );
}

rule!(
    struct NonEmptyObject:
    '{'
    {initializers: separated(rule_ref!(Initializer), ',')}
    {Repeat::at_most_once(',')}
    '}'
    => merge(expr!(initializers))
);
#[test]
fn non_empty_object() {
    let mut context = Context::default();
    let r = NonEmptyObject::rule();
    assert_eq!(
        r.parse("{a: 1}", &mut context).ast.unwrap(),
        json!({"a": 1})
    );
    assert_eq!(
        r.parse("{a: 1,}", &mut context).ast.unwrap(),
        json!({"a": 1})
    );
    assert_eq!(
        r.parse("{a: 1, b: 2}", &mut context).ast.unwrap(),
        json!({"a": 1, "b": 2})
    );
    assert_eq!(
        r.parse("{a: 1, b: 2,}", &mut context).ast.unwrap(),
        json!({"a": 1, "b": 2})
    );
}

rule!(struct Return: {value: Expression} => cast!(Return(value)));
#[test]
fn return_() {
    let mut context = Context::default();
    let r = Return::rule();
    assert_eq!(
        r.parse("1", &mut context).ast.unwrap(),
        json!({
            "Return": 1
        })
    );
}

rule!(struct Throw: "throw" {error: Expression} => cast!(Throw(error)));
#[test]
fn throw() {
    let mut context = Context::default();
    let r = Throw::rule();
    assert_eq!(
        r.parse("throw 1", &mut context).ast.unwrap(),
        json!({
            "Throw": 1
        })
    );
}

rule!(
    struct AtomicPattern: {
        alts!(
            seq!(
                '('
                {pattern: Pattern}
                ')'
                => pattern
            ),
            rule_ref!(Named),
            rule_ref!(RuleReference),
            rule_ref!(Regex),
            rule_ref!(Text)
        )
    }
);
#[test]
fn atomic_pattern() {
    let mut context = Context::default();
    let r = AtomicPattern::rule();
    assert_eq!(r.parse("(x)", &mut context).ast.unwrap(), json!("x"));
    assert_eq!(
        r.parse("<name: x>", &mut context).ast.unwrap(),
        json!({
            "Named": {
                "name": "name",
                "pattern": "x"
            }
        })
    );
    assert_eq!(
        r.parse("RuleName", &mut context).ast.unwrap(),
        json!({
            "RuleReference": "RuleName"
        })
    );
    assert_eq!(r.parse("/x/", &mut context).ast.unwrap(), json!("/x/"));
    assert_eq!(r.parse("x", &mut context).ast.unwrap(), json!("x"));
}

rule!(
    struct Alternatives: {
        separated(("x", rule_ref!(Sequence)), "|")
    }
);
#[test]
fn alternatives() {
    let mut context = Context::default();
    let r = Alternatives::rule();
    assert_eq!(r.parse("x", &mut context).ast.unwrap(), json!("x"));
    assert_eq!(
        r.parse("x | y", &mut context).ast.unwrap(),
        json!({"Alternatives": ["x", "y"]})
    );
    assert_eq!(
        r.parse("x y | z | f g", &mut context).ast.unwrap(),
        json!({"Alternatives": [["x", "y"], "z", ["f", "g"]]})
    );
}

rule!(
    struct DistinctObject:
        {ty: Typename}
        {obj: Object}
    => obj as ty
);
#[test]
fn distinct_object() {
    let mut context = Context::default();
    let r = DistinctObject::rule();
    assert_eq!(
        r.parse("Type {}", &mut context).ast.unwrap(),
        json!({
            "Type": {}
        })
    );
}

rule!(
    struct DistinctValue:
    {ty: Typename}
    '('
    {value: Value}
    ')'
    => value as ty
);
#[test]
fn distinct_value() {
    let mut context = Context::default();
    let r = DistinctValue::rule();
    assert_eq!(
        r.parse("Type(1)", &mut context).ast.unwrap(),
        json!({
            "Type": 1
        })
    );
    assert_eq!(
        r.parse("Type('c')", &mut context).ast.unwrap(),
        json!({
            "Type": 'c'
        })
    );
    assert_eq!(
        r.parse("Type(\"str\")", &mut context).ast.unwrap(),
        json!({
            "Type": "str"
        })
    );
    assert_eq!(
        r.parse("Type({})", &mut context).ast.unwrap(),
        json!({
            "Type": {}
        })
    );
    assert_eq!(
        r.parse("Type({a: 1})", &mut context).ast.unwrap(),
        json!({
            "Type": {"a": 1}
        })
    );
    assert_eq!(
        r.parse("Foo( Bar { a: 1 } )", &mut context).ast.unwrap(),
        json!({
            "Foo": {
                "Bar": {
                    "a": 1
                }
            }
        })
    );
}

rule!(
    struct Distinct: {
        alts!( DistinctObject | DistinctValue )
    }
);
#[test]
fn distinct() {
    let mut context = Context::default();
    let r = Distinct::rule();
    assert_eq!(
        r.parse("Type {}", &mut context).ast.unwrap(),
        json!({
            "Type": {}
        })
    );
    assert_eq!(
        r.parse("Type(1)", &mut context).ast.unwrap(),
        json!({
            "Type": 1
        })
    );
}

rule!(struct Expand: "..." {expr: Expression} => cast!(Expand(expr)));
#[test]
fn expand() {
    let mut context = Context::default();
    let r = Expand::rule();
    assert_eq!(
        r.parse("...a", &mut context).ast.unwrap(),
        json!({"Expand": { "Variable": "a"}})
    )
}

rule!(
    struct AtomicExpression: {
        alts!(
            seq!('(' {expr: Expression} ')' => expr),
            rule_ref!(Value),
            rule_ref!(Variable)
        )
    }
);
#[test]
fn atomic_expression() {
    let mut context = Context::default();
    let r = AtomicExpression::rule();
    assert_eq!(r.parse("(1)", &mut context).ast.unwrap(), json!(1));
    assert_eq!(r.parse("1", &mut context).ast.unwrap(), json!(1));
    assert_eq!(r.parse("true", &mut context).ast.unwrap(), json!(true));
    assert_eq!(r.parse("false", &mut context).ast.unwrap(), json!(false));
    assert_eq!(
        r.parse("a", &mut context).ast.unwrap(),
        json!({"Variable": "a"})
    );
}
