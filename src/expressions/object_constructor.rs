use derive_more::From;
use serde::{de::Error as SerdeError, Deserialize, Serialize};
use serde_json::{json, Map, Value};

use crate::{
    alts,
    bootstrap::rules::{DistinctObject, Expand, Identifier, Object},
    cast,
    errors::{CustomError, Error, Severity},
    rule, seq, Expression,
};

/// Initializes a field of an object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FieldInitializer {
    /// Name of the field. If none, the field name is the same as variable
    pub name: Option<String>,
    /// Value of the field
    pub value: Expression,
}
rule!(
    FieldInitializer: {
        alts!(
            seq!(
                {name: Identifier}
                ':'
                {value: Expression}
                => value as name
            ),
            seq!(
                {var: Identifier}
                => (cast!(Variable(var))) as var
            )
        )
    }
);

#[derive(Debug, PartialEq, Eq, Clone, From)]
pub enum Initializer {
    /// Initialize field
    FieldInitializer(FieldInitializer),
    /// Merge fields of one object into another object
    Expand(Expression),
}
rule!(Initializer: { alts!(FieldInitializer | Expand) });

/// Constructor for an object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ObjectConstructor {
    /// Type of the object, if any
    pub ty: Option<String>,
    /// Field initializers
    pub initializers: Vec<Initializer>,
}
rule!(
    ObjectConstructor: {
        alts!(
            DistinctObject | Object
        )
    }
);

impl ObjectConstructor {
    /// Cast this object as an expression to another type
    pub fn cast_to(self, ty: impl Into<Expression>) -> Expression {
        Expression::from(self).cast_to(ty)
    }

    pub fn evaluate(&self, variables: &Map<String, Value>) -> Result<Value, Error> {
        let mut obj = Map::new();
        for initializer in &self.initializers {
            match initializer {
                Initializer::FieldInitializer(field) => {
                    let value = field.value.evaluate(variables)?;
                    let name = field
                        .name
                        .clone()
                        .or_else(|| field.value.as_variable().map(|v| v.to_string()))
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
                Initializer::Expand(expr) => {
                    let mut value = expr.evaluate(variables)?;
                    obj.append(value.as_object_mut().unwrap());
                }
            }
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
            match i {
                Initializer::FieldInitializer(field) => {
                    let name = field
                        .name
                        .clone()
                        .unwrap_or_else(|| field.value.as_variable().unwrap().to_string());
                    obj.insert(name, json!(field.value));
                }
                Initializer::Expand(expr) => {
                    obj.insert("Expand".to_string(), json!(expr));
                }
            }
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
                obj = obj
                    .values()
                    .next()
                    .unwrap()
                    .as_object()
                    .ok_or(D::Error::custom("object is a distinct value"))?
                    .clone();
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
                .map(|(name, value)| {
                    if let Some("Expand") = name.as_ref().map(|n| n.as_str()) {
                        Initializer::Expand(value)
                    } else {
                        FieldInitializer { name, value }.into()
                    }
                })
                .collect(),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{obj, parser::Parser, Context, UnderlyingRule};
    use pretty_assertions::assert_eq;
    use serde_json::json;

    #[test]
    fn parse_field_initializer() {
        let mut context = Context::default();
        let r = FieldInitializer::rule();
        assert_eq!(r.parse("a: 1", &mut context).ast.unwrap(), json!({"a": 1}));
        assert_eq!(
            r.parse("a", &mut context).ast.unwrap(),
            json!({"a": { "Variable": "a" }})
        );
    }

    #[test]
    fn parse_object_constructor() {
        let mut context = Context::default();
        let r = ObjectConstructor::rule();
        assert_eq!(r.parse("{}", &mut context).ast.unwrap(), json!({}));
        assert_eq!(
            r.parse("Typed {}", &mut context).ast.unwrap(),
            json!({"Typed": {}})
        );
        assert_eq!(
            r.parse("{ a }", &mut context).ast.unwrap(),
            json!({ "a": { "Variable": "a" } })
        );
        assert_eq!(
            r.parse("Typed { a }", &mut context).ast.unwrap(),
            json!({"Typed": { "a": { "Variable": "a" }}})
        );
        assert_eq!(
            r.parse("{ a: 'c' }", &mut context).ast.unwrap(),
            json!({ "a": 'c' })
        );
        assert_eq!(
            r.parse("Typed { a: 'c' }", &mut context).ast.unwrap(),
            json!({"Typed": { "a": 'c' }})
        );
        assert_eq!(
            r.parse("{ ...a }", &mut context).ast.unwrap(),
            json!({ "Expand": { "Variable": "a" } })
        );
        assert_eq!(
            r.parse("Typed { ...a }", &mut context).ast.unwrap(),
            json!({"Typed": { "Expand": { "Variable": "a" }}})
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
