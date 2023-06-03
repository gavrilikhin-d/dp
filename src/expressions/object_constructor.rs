use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

use crate::{
    action::{reference, ret},
    alts,
    bootstrap::rules::{DistinctObject, Identifier, Object},
    errors::{CustomError, Error, Severity},
    patterns::transparent,
    rule_ref, seq, Expression, Rule,
};

/// Initializes a field of an object
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct FieldInitializer {
    /// Name of the field. If none, the field name is the same as variable
    pub name: Option<String>,
    /// Value of the field
    pub value: Expression,
}

impl FieldInitializer {
    pub fn rule() -> Rule {
        Rule::new(
            "FieldInitializer",
            transparent(alts!(
                seq!(
                    ("name", rule_ref!(Identifier)),
                    ':',
                    ("value", rule_ref!("Expression"))
                    =>
                    ret(reference("value").cast_to(reference("name")))
                ),
                seq!(
                    ("var", rule_ref!(Identifier))
                    =>
                    ret(
                        reference("var")
                            .cast_to("Variable")
                            .cast_to(reference("var"))
                    )
                )
            )),
        )
    }
}

/// Constructor for an object
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct ObjectConstructor {
    /// Type of the object, if any
    pub ty: Option<String>,
    /// Field initializers
    pub initializers: Vec<FieldInitializer>,
}

impl ObjectConstructor {
    pub fn evaluate(&self, variables: &Map<String, Value>) -> Result<Value, Error> {
        let mut obj = Map::new();
        for initializer in &self.initializers {
            let value = initializer.value.evaluate(variables)?;
            let name = initializer
                .name
                .clone()
                .or_else(|| initializer.value.as_variable().map(|v| v.to_string()))
                .ok_or_else(|| CustomError {
                    message: "Ommiting field name is only allowed for variable references"
                        .to_string(),
                    severity: Severity::Error,
                    code: None,
                    help: None,
                    labels: None,
                    url: None,
                })?;
            obj.insert(name, value);
        }
        Ok(obj.into())
    }

    pub fn rule() -> Rule {
        Rule::new(
            "ObjectConstructor",
            transparent(alts!(rule_ref!(DistinctObject), rule_ref!(Object))),
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{parsers::Parser, Context};
    use pretty_assertions::assert_eq;
    use serde_json::json;

    #[test]
    fn parse_field_initializer() {
        let mut context = Context::default();
        let r = FieldInitializer::rule();
        assert_eq!(r.parse("a: 1", &mut context).ast, json!({"a": 1}));
        assert_eq!(
            r.parse("a", &mut context).ast,
            json!({"a": { "Variable": "a" }})
        );
    }

    #[test]
    fn parse_object_constructor() {
        let mut context = Context::default();
        let r = ObjectConstructor::rule();
        assert_eq!(r.parse("{}", &mut context).ast, json!({}));
        assert_eq!(r.parse("Typed {}", &mut context).ast, json!({"Typed": {}}));
        assert_eq!(
            r.parse("{ a }", &mut context).ast,
            json!({ "a": { "Variable": "a" } })
        );
        assert_eq!(
            r.parse("Typed { a }", &mut context).ast,
            json!({"Typed": { "a": { "Variable": "a" }}})
        );
        assert_eq!(r.parse("{ a: 'c' }", &mut context).ast, json!({ "a": 'c' }));
        assert_eq!(
            r.parse("Typed { a: 'c' }", &mut context).ast,
            json!({"Typed": { "a": 'c' }})
        );
    }

    #[test]
    fn evaluate() {
        let obj = ObjectConstructor {
            ty: None,
            initializers: vec![
                FieldInitializer {
                    name: None,
                    value: Expression::Variable("a".to_string()),
                },
                FieldInitializer {
                    name: Some("b".to_string()),
                    value: json!(1).into(),
                },
            ],
        };

        let variables = json!({ "a": 0 });
        assert_eq!(
            obj.evaluate(&variables.as_object().unwrap()).unwrap(),
            json!({"a": 0, "b": 1})
        )
    }
}
