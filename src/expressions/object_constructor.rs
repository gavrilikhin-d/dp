use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

use crate::{
    alts,
    bootstrap::rules::{DistinctObject, Identifier, Object},
    errors::{CustomError, Error, Severity},
    expr,
    patterns::transparent,
    rule, rule_ref, seq, Expression,
};

/// Initializes a field of an object
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct FieldInitializer {
    /// Name of the field. If none, the field name is the same as variable
    pub name: Option<String>,
    /// Value of the field
    pub value: Expression,
}
rule!(
    FieldInitializer:
    transparent(alts!(
        seq!(
            ("name", rule_ref!(Identifier)),
            ':',
            ("value", rule_ref!("Expression"))
            => expr!(value).cast_to(expr!(name))
        ),
        seq!(
            ("var", rule_ref!(Identifier))
            =>
            expr!(var)
                .cast_to("Variable")
                .cast_to(expr!(var))
        )
    ))
);

/// Constructor for an object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectConstructor {
    /// Type of the object, if any
    pub ty: Option<String>,
    /// Field initializers
    pub initializers: Vec<FieldInitializer>,
}
rule!(
    ObjectConstructor:
    alts!(
        rule_ref!(DistinctObject),
        rule_ref!(Object)
    )
);

impl ObjectConstructor {
    /// Cast this object as an expression to another type
    pub fn cast_to(self, ty: impl Into<Expression>) -> Expression {
        Expression::from(self).cast_to(ty)
    }

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
        if let Some(ty) = &self.ty {
            let mut typed = Map::new();
            typed.insert(ty.clone(), obj.into());
            Ok(typed.into())
        } else {
            Ok(obj.into())
        }
    }
}

impl Serialize for ObjectConstructor {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut obj = Map::new();
        for i in &self.initializers {
            let name = i
                .name
                .clone()
                .unwrap_or_else(|| i.value.as_variable().unwrap().to_string());
            obj.insert(name, json!(i.value));
        }
        if let Some(ty) = &self.ty {
            let mut typed = Map::new();
            typed.insert(ty.clone(), obj.into());
            typed.serialize(serializer)
        } else {
            obj.serialize(serializer)
        }
    }
}

impl<'de> Deserialize<'de> for ObjectConstructor {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let mut ty = None;
        let mut obj = Map::deserialize(deserializer)?;
        if obj.len() == 1 {
            let key = obj.keys().next().unwrap().clone();
            if char::is_uppercase(key.chars().nth(0).unwrap()) {
                ty = Some(key);
                obj = obj.values().next().unwrap().as_object().unwrap().clone();
            }
        }

        Ok(Self {
            ty,
            initializers: obj
                .into_iter()
                .map(|(name, value)| (name, serde_json::from_value::<Expression>(value).unwrap()))
                .map(|(name, value)| {
                    if let Some(var) = value.as_variable() && var == name {
                        (None, value)
                    } else {
                        (Some(name), value)
                    }
                })
                .map(|(name, value)| FieldInitializer { name, value })
                .collect(),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{obj, parsers::Parser, Context, UnderlyingRule};
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
    fn serialize_object_constructor() {
        let mut obj = obj! { a, b: 1 };
        assert_eq!(
            json!(obj),
            json!({
                "a": {"Variable": "a"},
                "b": 1
            })
        );

        obj.ty = Some("Type".to_string());
        assert_eq!(
            json!(obj),
            json!({
                "Type": {
                    "a": {"Variable": "a"},
                    "b": 1
                }
            })
        )
    }

    #[test]
    fn deserialize_object_constructor() {
        let mut obj = obj! {a, b: 1};
        assert_eq!(
            obj,
            serde_json::from_value(json!({
                "a": {"Variable": "a"},
                "b": 1
            }))
            .unwrap()
        );

        obj.ty = Some("Type".to_string());
        assert_eq!(
            obj,
            serde_json::from_value(json!({
                "Type": {
                    "a": {"Variable": "a"},
                    "b": 1
                }
            }))
            .unwrap()
        )
    }

    #[test]
    fn evaluate() {
        let obj = obj! { a, b: 1 };

        let variables = json!({ "a": 0 });
        assert_eq!(
            obj.evaluate(&variables.as_object().unwrap()).unwrap(),
            json!({"a": 0, "b": 1})
        )
    }
}
