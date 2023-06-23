use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

use crate::{
    alts,
    bootstrap::rules::{Return, Throw},
    errors::Error,
    rule, Expression,
};

/// Action to do on AST
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum Action {
    /// Throw value as an error
    Throw(Expression),
    /// Return value
    Return(Expression),
}
rule!(
    Action:
        "=>"
        {value: alts!(Throw | Return)}
        => value
);

impl Action {
    /// Execute this action with expanding variables
    pub fn execute(&self, variables: &Map<String, Value>) -> Result<Value, Error> {
        match self {
            Action::Throw(expr) => Err(serde_json::from_value(expr.evaluate(variables)?).unwrap()),
            Action::Return(expr) => Ok(expr.evaluate(variables)?),
        }
    }
}

/// A JSON value that represents a variable reference.
pub fn reference(name: &str) -> Expression {
    Expression::Variable(name.to_string())
}

/// Create a throw action
pub fn throw(expr: impl Into<Expression>) -> Action {
    Action::Throw(expr.into())
}

/// Create a return action
pub fn ret(expr: impl Into<Expression>) -> Action {
    Action::Return(expr.into())
}

/// Merge an array of objects into one object
pub fn merge(expr: impl Into<Expression>) -> Expression {
    Expression::Merge(Box::new(expr.into()))
}

#[cfg(test)]
mod test {
    use crate::{action::Action, parser::Parser, Context, UnderlyingRule};
    use pretty_assertions::assert_eq;
    use serde_json::json;

    #[test]
    fn action() {
        let mut context = Context::default();
        let r = Action::rule();
        assert_eq!(
            r.parse("=> 1", &mut context).unwrap().ast,
            json!({
                "Return": 1
            })
        );
        assert_eq!(
            r.parse("=> throw 1", &mut context).unwrap().ast,
            json!({
                "Throw": 1
            })
        );
    }
}
