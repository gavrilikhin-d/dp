use derive_more::From;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

use crate::{alts, bootstrap::rules::Variable, errors::Error, rule_ref, Rule};

mod cast;
pub use cast::*;

mod object_constructor;
pub use object_constructor::*;

/// Expression that can be evaluated to a value
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, From)]
pub enum Expression {
    /// A variable reference
    #[from(ignore)]
    Variable(String),
    /// Flatten an array of expressions
    #[from(ignore)]
    Flatten(Vec<Expression>),
    /// Merge an array of objects into one object
    #[from(ignore)]
    Merge(Box<Expression>),
    /// A cast from one type to another
    Cast(Box<Cast>),
    /// An object constructor
    #[serde(untagged)]
    ObjectConstructor(ObjectConstructor),
    /// A JSON value
    #[serde(untagged)]
    Value(Value),
}

impl From<i32> for Expression {
    fn from(value: i32) -> Self {
        Expression::Value(value.into())
    }
}

impl From<&str> for Expression {
    fn from(value: &str) -> Self {
        Expression::Value(value.into())
    }
}

impl Expression {
    /// Evaluate expression to value
    pub fn evaluate(&self, variables: &Map<String, Value>) -> Result<Value, Error> {
        match self {
            Expression::Value(value) => Ok(value.clone()),
            Expression::Flatten(values) => {
                let values = values
                    .iter()
                    .map(|v| v.evaluate(variables))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut result = Vec::new();
                for value in values {
                    if let Value::Array(arr) = value {
                        for value in arr {
                            result.push(value);
                        }
                    } else {
                        result.push(value);
                    }
                }
                Ok(result.into())
            }
            Expression::Merge(expr) => {
                let mut v = expr.evaluate(variables)?;
                let objs = v.as_array_mut().unwrap();
                let mut result = Map::new();
                for obj in objs {
                    result.append(obj.as_object_mut().unwrap());
                }
                Ok(result.into())
            }
            Expression::Variable(name) => {
                let value = variables
                    .get(name)
                    .expect(format!("Variable {:?} not found", name).as_str());
                Ok(value.clone())
            }
            Expression::Cast(cast) => {
                let value = cast.expr.evaluate(variables)?;
                let ty = cast.ty.evaluate(variables)?;
                Ok(json!({ ty.as_str().unwrap(): value }))
            }
            Expression::ObjectConstructor(oc) => oc.evaluate(variables),
        }
    }

    /// Get the name of the variable, if this expression is a variable
    pub fn as_variable(&self) -> Option<&str> {
        match self {
            Expression::Variable(name) => Some(name),
            _ => None,
        }
    }

    /// Equivalent to:
    /// ```text
    /// <expr: Expression> as <ty: Type>
    /// ```
    pub fn cast_to(self, ty: impl Into<Expression>) -> Expression {
        Expression::Cast(Box::new(Cast {
            expr: self,
            ty: ty.into(),
        }))
    }

    pub fn rule() -> Rule {
        Rule::new(
            "Expression",
            alts!(
                rule_ref!(Cast),
                rule_ref!(crate::bootstrap::rules::Value),
                rule_ref!(Variable)
            ),
        )
    }
}

#[cfg(test)]
mod test {
    use serde_json::json;

    use pretty_assertions::assert_eq;

    use crate::{parsers::Parser, Context, Expression};

    #[test]
    fn expression() {
        let mut context = Context::default();
        let r = Expression::rule();
        assert_eq!(r.parse("1", &mut context).ast, json!(1));
        assert_eq!(r.parse("var", &mut context).ast, json!({"Variable": "var"}));
        assert_eq!(
            r.parse("1 as Integer", &mut context).ast,
            json!({
                "Cast": {
                    "ty": "Integer",
                    "expr": 1
                }
            })
        );
    }
}
