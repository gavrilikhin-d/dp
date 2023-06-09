mod named;
use colored::Colorize;
use log::trace;
pub use named::*;

mod repeat;
pub use repeat::*;

mod sequence;
pub use sequence::*;

mod rule_ref;
pub use rule_ref::*;

use derive_more::From;

use regex::Regex;

use serde::{Deserialize, Serialize};

use crate::{
    arr,
    bootstrap::rules::{Alternatives, Whitespace},
    errors::Expected,
    log::{log_with_highlight, target_line_column},
    parser::{ParseResult, Parser},
    rule, rule_ref, seq,
    syntax::token::Kind,
    Context,
};

/// Possible patterns
#[derive(Debug, PartialEq, Eq, Clone, From)]
pub enum Pattern {
    /// Reference to another rule
    RuleReference(Box<RuleReference>),
    /// Sequence of patterns
    #[from(ignore)]
    Sequence(Sequence),
    /// Match specific text
    #[from(ignore)]
    Text(String),
    /// Regex expression
    #[from(ignore)]
    Regex(String),
    /// Pattern alternatives
    #[from(ignore)]
    Alternatives(Vec<Pattern>),
    /// Repeat pattern
    Repeat(Repeat),
    /// Adds name to the ast of pattern
    Named(Named),
}
rule!(Pattern: Alternatives);

impl From<Sequence> for Pattern {
    fn from(value: Sequence) -> Self {
        if value.action.is_none() && value.patterns.len() == 1 {
            value.patterns.into_iter().next().unwrap()
        } else {
            Pattern::Sequence(value)
        }
    }
}

/// <head: Pattern> <tail: (Separator (<value: Pattern> => value))*> => [head, ...tail]
pub fn separated(pattern: impl Into<Pattern>, separator: impl Into<Pattern>) -> Pattern {
    let pattern = pattern.into();
    seq!(
        {head: pattern.clone()}
        {
            tail:
            Repeat::zero_or_more(
                seq!(
                    {separator.into()}
                    {value: pattern.clone()}
                    => value
                )
            )
        }
        => arr! [head, ...tail]
    )
    .into()
}

impl Pattern {
    /// Return an alternative pattern between this pattern and another
    pub fn or(mut self, other: Pattern) -> Self {
        match &mut self {
            Pattern::Alternatives(alts) => alts.push(other),
            _ => {
                self = Pattern::Alternatives(vec![self, other]);
            }
        }
        self
    }

    /// Is this pattern a sequence?
    pub fn is_sequence(&self) -> bool {
        matches!(self, Pattern::Sequence(_))
    }

    /// Cast this pattern to a sequence
    pub fn as_sequence(&self) -> Option<&Sequence> {
        match self {
            Pattern::Sequence(s) => Some(s),
            _ => None,
        }
    }

    /// Is this pattern a named pattern?
    pub fn is_named(&self) -> bool {
        matches!(self, Pattern::Named(_))
    }

    /// Cast this pattern to a named pattern
    pub fn as_named(&self) -> Option<&Named> {
        match self {
            Pattern::Named(n) => Some(n),
            _ => None,
        }
    }
}

impl Serialize for Pattern {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        PatternDTO::from(self.clone()).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Pattern {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let dto = PatternDTO::deserialize(deserializer)?;
        Ok(dto.into())
    }
}

#[derive(Serialize, Deserialize, From)]
enum PatternDTO {
    RuleReference(Box<RuleReference>),
    #[from(ignore)]
    Alternatives(Vec<Pattern>),
    Repeat(Repeat),
    Named(Named),
    Sequence(Sequence),

    #[serde(untagged)]
    TextOrRegex(String),
    #[serde(untagged)]
    SequenceWithoutAction(Vec<Pattern>),
}

impl From<Pattern> for PatternDTO {
    fn from(value: Pattern) -> Self {
        match value {
            Pattern::Text(t) => PatternDTO::TextOrRegex(t),
            Pattern::Regex(r) => PatternDTO::TextOrRegex(format!("/{}/", r)),
            Pattern::Sequence(s) => {
                if s.action.is_none() {
                    PatternDTO::SequenceWithoutAction(s.patterns)
                } else {
                    PatternDTO::Sequence(s).into()
                }
            }

            Pattern::RuleReference(r) => PatternDTO::RuleReference(r).into(),
            Pattern::Named(named) => PatternDTO::Named(named).into(),
            Pattern::Repeat(r) => PatternDTO::Repeat(r).into(),
            Pattern::Alternatives(alts) => {
                PatternDTO::Alternatives(alts.into_iter().map(|a| a.into()).collect()).into()
            }
        }
    }
}

impl From<PatternDTO> for Pattern {
    fn from(value: PatternDTO) -> Self {
        match value {
            PatternDTO::TextOrRegex(t) => t.into(),
            PatternDTO::Sequence(s) => s.into(),
            PatternDTO::RuleReference(r) => r.into(),
            PatternDTO::Named(n) => n.into(),
            PatternDTO::Repeat(r) => r.into(),
            PatternDTO::Alternatives(alts) => Pattern::Alternatives(alts),
            PatternDTO::SequenceWithoutAction(s) => s.into(),
        }
    }
}

impl From<char> for Pattern {
    fn from(value: char) -> Self {
        Pattern::Text(value.to_string())
    }
}

impl From<&str> for Pattern {
    fn from(s: &str) -> Self {
        if s.len() > 1 && s.starts_with('/') && s.ends_with('/') {
            return Pattern::Regex(s[1..s.len() - 1].to_string());
        }
        Pattern::Text(s.into())
    }
}

impl From<String> for Pattern {
    fn from(s: String) -> Self {
        s.as_str().into()
    }
}

impl From<Vec<Pattern>> for Pattern {
    fn from(value: Vec<Pattern>) -> Self {
        Pattern::Sequence(value.into())
    }
}

impl<N: Into<String>, P: Into<Pattern>> From<(N, P)> for Pattern {
    fn from(value: (N, P)) -> Self {
        Pattern::Named(value.into())
    }
}

impl Parser for Pattern {
    fn parse_at<'s>(&self, source: &'s str, at: usize, context: &mut Context) -> ParseResult {
        match self {
            Pattern::Text(text) => {
                Pattern::Regex(regex::escape(text)).parse_at(source, at, context)
            }
            Pattern::Regex(r) => {
                let target =
                    target_line_column(format!("/{}/", r.escape_default()).as_str(), at, source);

                // Find length of whitespace characters sequence
                let mut trivia_size = 0;

                let try_parse = |trivia_size: usize| {
                    let re = Regex::new(format!("^{r}").as_str()).expect("Invalid regex");
                    let m = re.find(&source[at + trivia_size..]).map(|m| m.as_str());
                    if m.is_none() {
                        trace!(target: target.as_str(), "{}", "ERR".red().bold());
                        return ParseResult {
                            syntax: Expected {
                                expected: r.clone(),
                                at: at.into(),
                            }
                            .into(),
                            ast: None,
                        };
                    }

                    let m = m.unwrap();
                    let start = at + trivia_size;
                    let end = start + m.len();
                    log_with_highlight(target.as_str(), at, source, start..end);

                    // TODO: use action to set token kind
                    let token_kind = if m.starts_with("/") && m.ends_with("/") {
                        Kind::Regexp
                    } else if m.starts_with("\"") && m.ends_with("\"") {
                        Kind::String
                    } else if m.starts_with("'") && m.ends_with("'") {
                        Kind::String
                    } else if m.chars().all(|c| c.is_numeric()) {
                        Kind::Number
                    } else if m.chars().all(|c| c.is_ascii_punctuation()) {
                        Kind::Operator
                    } else if m.chars().next().is_some_and(|c| c.is_uppercase()) {
                        Kind::Type
                    } else {
                        Kind::Keyword
                    };

                    ParseResult {
                        syntax: (token_kind, start..end).into(),
                        ast: Some(m.into()),
                    }
                };

                let result = try_parse(trivia_size);
                if result.ast.is_some() || !context.skip_whitespace {
                    return result;
                }

                context.skip_whitespace = false;
                trivia_size = rule_ref!(Whitespace)
                    .parse_at(source, at, context)
                    .syntax
                    .range()
                    .map_or(0, |r| r.len());
                context.skip_whitespace = true;

                try_parse(trivia_size)
            }
            Pattern::RuleReference(r) => r.parse_at(source, at, context),
            Pattern::Sequence(s) => s.parse_at(source, at, context),
            Pattern::Alternatives(alts) => {
                debug_assert!(alts.len() >= 2);

                let mut res = alts[0].parse_at(source, at, context);
                if res.ast.is_some() {
                    return res;
                }

                for alt in alts[1..].iter() {
                    res = alt.parse_at(source, at, context);
                    if res.ast.is_some() {
                        break;
                    }
                }
                res
            }
            Pattern::Repeat(r) => r.parse_at(source, at, context),
            Pattern::Named(n) => n.parse_at(source, at, context),
        }
    }
}

#[cfg(test)]
mod test {
    use serde_json::json;

    use pretty_assertions::assert_eq;

    use crate::{
        bootstrap::rules::Text,
        errors::Expected,
        parser::{ParseResult, Parser},
        patterns::Named,
        syntax::{self, token::Kind, Node},
        Context, Pattern,
    };

    #[test]
    fn text() {
        let mut context = Context::default();
        let pattern: Pattern = "text".into();
        assert_eq!(pattern, Pattern::Text("text".into()));
        assert_eq!(
            pattern.parse("text", &mut context),
            ParseResult {
                syntax: (Kind::Keyword, 0..4).into(),
                ast: Some(json!("text"))
            }
        );
    }

    #[test]
    fn regex() {
        let mut context = Context::default();
        let pattern: Pattern = r"/[^\s]+/".into();
        assert_eq!(pattern, Pattern::Regex(r"[^\s]+".into()));
        assert_eq!(
            pattern.parse("hello world", &mut context),
            ParseResult {
                syntax: (Kind::Keyword, 0..5).into(),
                ast: Some(json!("hello"))
            }
        );
    }

    #[test]
    fn alt() {
        let mut context = Context::default();
        let pattern = Pattern::Alternatives(vec!["a".into(), "bc".into()]);
        assert_eq!(
            pattern.parse("a", &mut context),
            ParseResult {
                syntax: (Kind::Keyword, 0..1).into(),
                ast: Some(json!("a"))
            }
        );
        assert_eq!(
            pattern.parse("bc", &mut context),
            ParseResult {
                syntax: (Kind::Keyword, 0..2).into(),
                ast: Some(json!("bc"))
            }
        );
        assert_eq!(
            pattern.parse("c", &mut context).syntax,
            Expected {
                expected: "bc".to_string(),
                at: 0
            }
            .into()
        );
    }

    #[test]
    fn sequence() {
        let mut context = Context::default();
        let pattern = Pattern::Sequence(vec!["a".into(), "b".into()].into());
        assert_eq!(
            pattern.parse("ab", &mut context),
            ParseResult {
                syntax: vec![(Kind::Keyword, 0..1).into(), (Kind::Keyword, 1..2).into()].into(),
                ast: Some(json!({}))
            }
        );
        assert_eq!(
            pattern.parse("b", &mut context).syntax,
            Expected {
                expected: "a".to_string(),
                at: 0
            }
            .into()
        );
        assert_eq!(
            pattern.parse("a", &mut context).syntax,
            Expected {
                expected: "b".to_string(),
                at: 1
            }
            .into()
        );
        assert_eq!(
            pattern.parse("", &mut context).syntax,
            Expected {
                expected: "a".to_string(),
                at: 0
            }
            .into()
        )
    }

    #[test]
    fn rule_ref() {
        let mut context = Context::default();
        let pattern = crate::rule_ref!(Text);
        assert_eq!(
            pattern.parse("abc", &mut context),
            ParseResult {
                syntax: Node::from((Kind::Keyword, 0..3)).with_name("Text"),
                ast: Some(json!("abc"))
            }
        )
    }

    #[test]
    fn named() {
        use crate::Context;

        let mut context = Context::default();
        let pattern: Pattern = Named {
            name: "name".to_string(),
            pattern: Box::new("/[A-z][a-z]*/".into()),
        }
        .into();
        assert_eq!(
            pattern.parse("John", &mut context),
            ParseResult {
                syntax: syntax::Node::Named {
                    name: "name".to_string(),
                    // FIXME: Kind::Keyword
                    node: Box::new((Kind::Type, 0..4).into())
                },
                ast: Some(json!({"name": "John"}))
            }
        );
    }
}
