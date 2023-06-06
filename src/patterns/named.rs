use derive_more::From;
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{
    bootstrap::rules::Identifier,
    parsers::{ParseResult, Parser},
    rule, Context, Pattern,
};

/// Adds name to the ast of pattern
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone, From)]
pub struct Named {
    /// Name to add to the ast
    pub name: String,
    /// Pattern itself
    pub pattern: Box<Pattern>,
}

rule!(
    Named:
        '<'
        {name: Identifier}
        ':'
        {pattern: Pattern}
        '>'
);

#[test]
fn named() {
    use crate::UnderlyingRule;
    let mut context = Context::default();
    let r = Named::rule();
    assert_eq!(
        r.parse("<name: x>", &mut context).ast,
        json!({
            "Named": {
                "name": "name",
                "pattern": "x"
            }
        })
    );
}

impl Parser for Named {
    fn parse_at<'s>(&self, source: &'s str, at: usize, context: &mut Context) -> ParseResult<'s> {
        let mut result = self.pattern.parse_at(source, at, context);
        result.ast = json!({&self.name: result.ast});
        result
    }
}

impl<N: Into<String>, P: Into<Pattern>> From<(N, P)> for Named {
    fn from(value: (N, P)) -> Self {
        Self {
            name: value.0.into(),
            pattern: Box::new(value.1.into()),
        }
    }
}

#[test]
fn test_named() {
    use crate::parsers::ParseResult;
    use crate::Context;
    use pretty_assertions::assert_eq;

    let mut context = Context::default();
    let pattern = Named {
        name: "name".to_string(),
        pattern: Box::new("/[A-z][a-z]*/".into()),
    };
    assert_eq!(
        pattern.parse("John", &mut context),
        ParseResult {
            delta: 4,
            tree: "John".into(),
            ast: json!({"name": "John"}),
        }
    );
}

#[test]
fn deserialize() {
    use pretty_assertions::assert_eq;

    let named = Named {
        name: "name".to_string(),
        pattern: Box::new("/[a-z]+/".into()),
    };
    let json = json!({"name": "name", "pattern": "/[a-z]+/"});
    assert_eq!(named, serde_json::from_value(json.clone()).unwrap());
    assert_eq!(
        Pattern::Named(named),
        serde_json::from_value(json!({ "Named": json })).unwrap()
    )
}
