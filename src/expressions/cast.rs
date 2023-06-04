use serde::{Deserialize, Serialize};

use crate::{
    alts,
    bootstrap::rules::{Type, Value, Variable},
    rule, rule_ref, seq, Expression,
};

/// Cast expression to type
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Cast {
    /// Expression to cast
    pub expr: Expression,
    /// Type to cast to
    pub ty: Expression,
}
rule!(
    Cast:
    seq!(
        ("expr", alts!(rule_ref!(Variable), rule_ref!(Value))),
        "as",
        ("ty", rule_ref!(Type))
    )
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parsers::Parser, Context, UnderlyingRule};
    use pretty_assertions::assert_eq;
    use serde_json::json;

    #[test]
    fn cast() {
        let mut context = Context::default();
        let r = Cast::rule();
        assert_eq!(
            r.parse("1 as Integer", &mut context).ast,
            json!({
                "Cast": {
                    "ty": "Integer",
                    "expr": 1
                }
            })
        );
        assert_eq!(
            r.parse("1 as ty", &mut context).ast,
            json!({
                "Cast": {
                    "ty": { "Variable": "ty" },
                    "expr": 1
                }
            })
        );
    }
}
