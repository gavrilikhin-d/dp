use derive_more::From;
use serde::{Deserialize, Serialize};

use crate::{
    parser::{ParseResult, Parser},
    Rule,
};

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, From)]
#[serde(untagged)]
pub enum RuleReference {
    Weak(String),
    Strong(Rule),
}

impl From<&str> for RuleReference {
    fn from(value: &str) -> Self {
        Self::Weak(value.to_string())
    }
}

impl Parser for RuleReference {
    fn parse_at<'s>(
        &self,
        source: &'s str,
        at: usize,
        context: &mut crate::Context,
    ) -> ParseResult {
        match self {
            Self::Weak(name) => {
                let rule = context
                    .find_rule(name)
                    .expect(format!("Rule {name:?} not found").as_str());
                rule.parse_at(source, at, context)
            }
            Self::Strong(rule) => rule.parse_at(source, at, context),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{bootstrap::rules::Text, rule_ref, Context, Pattern};
    use pretty_assertions::assert_eq;
    use serde_json::json;

    #[test]
    fn weak() {
        let mut context = Context::default();
        let r = rule_ref!("Text");
        assert_eq!(r, Pattern::RuleReference(Box::new("Text".into())));
        assert_eq!(r.parse("text", &mut context).ast.unwrap(), json!("text"));

        let r = rule_ref!(Text);
        assert_eq!(r, Pattern::RuleReference(Box::new("Text".into())));
        assert_eq!(r.parse("text", &mut context).ast.unwrap(), json!("text"));
    }
}
