use serde_json::Value;

use crate::{errors::Error, syntax, Context};

/// Result of parsing
#[derive(Debug, PartialEq, Clone)]
pub struct ParseOk {
    /// Syntax tree
    pub syntax: syntax::Node,
    /// AST
    pub ast: Value,
}

pub type ParseResult = Result<ParseOk, Error>;

/// Parse source code starting at given position
pub trait Parser {
    /// Parse source code starting at given position
    fn parse_at<'s>(&self, source: &'s str, at: usize, context: &mut Context) -> ParseResult;

    /// Parse source code from the beginning
    fn parse<'s>(&self, source: &'s str, context: &mut Context) -> ParseResult {
        self.parse_at(source, 0, context)
    }
}
