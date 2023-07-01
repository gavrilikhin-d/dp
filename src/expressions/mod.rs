use derive_more::From;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

use crate::{
    alts,
    bootstrap::rules::AtomicExpression,
    errors::{Error, UndefinedVariable},
    rule,
};

mod cast;
pub use cast::*;

mod condition;
pub use condition::*;

mod object_constructor;
pub use object_constructor::*;

mod array_constructor;
pub use array_constructor::*;

/// Expression that can be evaluated to a value
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, From)]
pub enum Expression {
    /// A variable reference
    #[from(ignore)]
    Variable(String),
    /// Merge an array of objects into one object
    #[from(ignore)]
    Merge(Box<Expression>),
    /// A cast from one type to another
    Cast(Box<Cast>),
    /// If-expression
    If(Box<If>),
    /// An object constructor
    #[serde(untagged)]
    ObjectConstructor(ObjectConstructor),
    /// An array constructor
    #[serde(untagged)]
    ArrayConstructor(ArrayConstructor),
    /// A JSON value
    #[serde(untagged)]
    Value(Value),
}
rule!(Expression: {alts!(Cast | If | AtomicExpression)});

impl From<char> for Expression {
    fn from(value: char) -> Self {
        Expression::Value(value.to_string().into())
    }
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
                    .ok_or_else(|| UndefinedVariable { name: name.clone() })?;
                Ok(value.clone())
            }
            Expression::Cast(cast) => {
                let value = cast.expr.evaluate(variables)?;
                let ty = cast.ty.evaluate(variables)?;
                Ok(json!({ ty.as_str().unwrap(): value }))
            }
            Expression::ObjectConstructor(oc) => oc.evaluate(variables),
            Expression::ArrayConstructor(ac) => ac.evaluate(variables),
            Expression::If(check) => check.evaluate(variables),
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
}

#[cfg(test)]
mod test {
    use serde_json::{json, Map};

    use pretty_assertions::assert_eq;

    use crate::{
        errors::UndefinedVariable, expr, parser::Parser, Context, Expression, UnderlyingRule,
    };

    #[test]
    fn expression() {
        let mut context = Context::default();
        let r = Expression::rule();
        assert_eq!(r.parse("1", &mut context).ast.unwrap(), json!(1));
        assert_eq!(
            r.parse("var", &mut context).ast.unwrap(),
            json!({"Variable": "var"})
        );
        assert_eq!(
            r.parse("1 as Integer", &mut context).ast.unwrap(),
            json!({
                "Cast": {
                    "ty": "Integer",
                    "expr": 1
                }
            })
        );
    }

    #[test]
    fn undefined_variable() {
        assert_eq!(
            expr!(var).evaluate(&Map::new()),
            Err(UndefinedVariable {
                name: "var".to_string(),
            }
            .into())
        )
    }
}
