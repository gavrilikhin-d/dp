use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

use crate::{errors::Error, patterns::Repeat, rule, seq, Expression};

/// if-expression
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct If {
    /// Condition to evaluate
    pub condition: Expression,
    /// Code to run when condition is true
    pub on_true: Expression,
    /// Code to run when condition is false
    #[serde(default)]
    pub on_false: Option<Expression>,
}
rule!(
    If:
    "if" {condition: Expression}
    '{' {on_true: Expression} '}'
    {on_false: {
        Repeat::at_most_once(seq!("else" '{' {expr: Expression} '}' => expr))
    }}
);

impl If {
    /// Evaluate expression to value
    pub fn evaluate(&self, variables: &Map<String, Value>) -> Result<Value, Error> {
        let condition = self.condition.evaluate(variables)?;
        match condition {
            Value::Bool(true) => self.on_true.evaluate(variables),
            Value::Bool(false) => self
                .on_false
                .as_ref()
                .map(|e| e.evaluate(variables))
                .unwrap_or(Ok(json!(null))),
            _ => panic!("condition is not bool"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{expr, parser::Parser, Context, UnderlyingRule};

    use super::If;

    use pretty_assertions::assert_eq;
    use serde_json::json;

    #[test]
    fn parse() {
        let mut context = Context::default();
        let r = If::rule();
        assert_eq!(
            r.parse("if true { true }", &mut context).ast.unwrap(),
            json!({
                "If": {
                    "condition": true,
                    "on_true": true,
                    "on_false": null
                }
            })
        );
        assert_eq!(
            r.parse("if true { true } else { false }", &mut context)
                .ast
                .unwrap(),
            json!({
                "If": {
                    "condition": true,
                    "on_true": true,
                    "on_false": false
                }
            })
        );
    }

    #[test]
    fn condition() {
        let if_expr = If {
            condition: expr!(var),
            on_true: expr!(1),
            on_false: Some(expr!(2)),
        };

        let vars = json!({ "var": true });

        assert_eq!(if_expr.evaluate(vars.as_object().unwrap()), Ok(json!(1)));

        let vars = json!({ "var": false });
        assert_eq!(if_expr.evaluate(vars.as_object().unwrap()), Ok(json!(2)));
    }
}
