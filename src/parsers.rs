use serde_json::Value;

use crate::{errors::Error, Context};

/// Result of parsing
#[derive(Debug, PartialEq)]
pub struct ParseResult {
    /// Number of parsed characters
    pub delta: usize,
    /// AST
    pub ast: Value,
}

impl ParseResult {
    /// Create empty parse result
    pub fn empty() -> Self {
        Self {
            delta: 0,
            ast: Value::Null,
        }
    }

    /// Create parse result with AST
    pub fn with_ast(self, ast: Value) -> Self {
        Self { ast, ..self }
    }
}

/// Parse source code starting at given position
pub trait Parser {
    /// Parse source code starting at given position
    fn parse_at<'s>(
        &self,
        source: &'s str,
        at: usize,
        context: &mut Context,
    ) -> Result<ParseResult, Error>;

    /// Parse source code from the beginning
    fn parse<'s>(&self, source: &'s str, context: &mut Context) -> Result<ParseResult, Error> {
        self.parse_at(source, 0, context)
    }
}
