use derive_more::From;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

use crate::{
    alts, bootstrap::rules::Expand, errors::Error, patterns::separated, rule, rule_ref, seq,
    Expression,
};

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, From)]
pub enum ArrayElement {
    /// Expand an array into this array
    #[from]
    Expand(Expression),
    /// Add expressions as is
    #[serde(untagged)]
    Expression(Expression),
}
rule!(ArrayElement: { alts!(Expand | Expression) });

impl ArrayElement {
    pub fn evaluate(&self, variables: &Map<String, Value>) -> Result<Value, Error> {
        match self {
            Self::Expression(expr) | Self::Expand(expr) => expr.evaluate(variables),
        }
    }
}

/// Constructor for an object
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct ArrayConstructor(pub Vec<ArrayElement>);
rule!(
    ArrayConstructor: {
        alts!(
            seq!('[' ']' => json!([])),
            seq!('[' {elements: separated(rule_ref!(ArrayElement), ',')} ']' => elements)
        )
    }
);

impl ArrayConstructor {
    /// Cast this object as an expression to another type
    pub fn cast_to(self, ty: impl Into<Expression>) -> Expression {
        Expression::from(self).cast_to(ty)
    }

    pub fn evaluate(&self, variables: &Map<String, Value>) -> Result<Value, Error> {
        let mut arr = Vec::new();
        for initializer in &self.0 {
            let mut value = initializer.evaluate(variables)?;
            if let ArrayElement::Expand(_) = initializer {
                if let Some(values) = value.as_array_mut() {
                    arr.append(values);
                    continue;
                }
            }
            arr.push(value);
        }
        Ok(arr.into())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{arr, parsers::Parser, Context, UnderlyingRule};
    use pretty_assertions::assert_eq;
    use serde_json::json;

    #[test]
    fn parse_array_element() {
        let mut context = Context::default();
        let r = ArrayElement::rule();
        assert_eq!(r.parse("1", &mut context).unwrap().ast, json!(1));
        assert_eq!(
            r.parse("...a", &mut context).unwrap().ast,
            json!({"Expand": { "Variable": "a" }})
        );
    }

    #[test]
    fn parse_array_constructor() {
        let mut context = Context::default();
        let r = ArrayConstructor::rule();
        assert_eq!(r.parse("[]", &mut context).unwrap().ast, json!([]));
        assert_eq!(
            r.parse("[ a ]", &mut context).unwrap().ast,
            json!([{"Variable": "a" }])
        );
        assert_eq!(
            r.parse("[ 1, ...a, 2 ]", &mut context).unwrap().ast,
            json!([1, {"Expand": { "Variable": "a" }}, 2])
        );
    }

    #[test]
    fn evaluate() {
        let arr = arr! [ a, 1, ...b ];

        let variables = json!({ "a": 0, "b": [2] });
        assert_eq!(
            arr.evaluate(&variables.as_object().unwrap()).unwrap(),
            json!([0, 1, 2])
        )
    }
}
