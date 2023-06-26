use serde::{Deserialize, Serialize};

use crate::{
    alts,
    bootstrap::rules::{AtomicExpression, Type, Typename},
    obj, rule, seq, Expression,
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
    Cast: {
        alts!(
            seq!(
                {expr: AtomicExpression} "as" {ty: Type} => obj!(Cast { expr, ty })
            ),
            seq!(
                {ty: Typename} '(' {expr: Expression} ')' => obj!(Cast { expr, ty })
            )
        )
    }
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::Parser, Context, UnderlyingRule};
    use pretty_assertions::assert_eq;
    use serde_json::json;

    #[test]
    fn cast() {
        let mut context = Context::default();
        let r = Cast::rule();
        assert_eq!(
            r.parse("1 as Integer", &mut context).ast.unwrap(),
            json!({
                "Cast": {
                    "ty": "Integer",
                    "expr": 1
                }
            })
        );
        assert_eq!(
            r.parse("1 as ty", &mut context).ast.unwrap(),
            json!({
                "Cast": {
                    "ty": { "Variable": "ty" },
                    "expr": 1
                }
            })
        );
        assert_eq!(
            r.parse("Ty(var)", &mut context).ast.unwrap(),
            json!({
                "Cast": {
                    "ty": "Ty",
                    "expr": {
                        "Variable": "var"
                    }
                }
            })
        );
        assert_eq!(
            r.parse("(var as X) as Y", &mut context).ast.unwrap(),
            json!({
                "Cast": {
                    "ty": "Y",
                    "expr": {
                        "Cast": {
                            "ty": "X",
                            "expr": {
                                "Variable": "var"
                            }
                        }
                    }
                }
            })
        );
    }
}
