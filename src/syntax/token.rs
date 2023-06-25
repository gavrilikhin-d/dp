use std::ops::Range;

/// Kind of token
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Keyword,
    Operator,
    Comment,
    Regexp,
    String,
    Number,
    Type,
    Parameter,
}

/// Modifier for token
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Modifier {
    Definition,
}

/// Token in the syntax tree
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// Kind of token
    pub kind: Kind,
    /// Range of token
    pub range: Range<usize>,
    /// Modifiers for token
    pub modifiers: Vec<Modifier>,
}

impl Token {
    pub fn new(kind: Kind, range: impl Into<Range<usize>>) -> Self {
        Self {
            kind,
            range: range.into(),
            modifiers: vec![],
        }
    }
}

impl From<(Kind, Range<usize>)> for Token {
    fn from((kind, range): (Kind, Range<usize>)) -> Self {
        Self::new(kind, range)
    }
}
