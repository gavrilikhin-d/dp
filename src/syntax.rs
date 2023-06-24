use std::ops::Range;

use derive_more::From;

use crate::errors::Error;

/// Syntax tree node
#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum Node {
    /// Named node in the syntax tree
    Named { name: String, node: Box<Node> },
    /// Token in the syntax tree
    Unnamed(Vec<Node>),
    /// Leaf token in the syntax tree
    Token(Range<usize>),
    /// Error in the syntax tree
    #[from(ignore)]
    Error(Error),
}

impl Node {
    /// Iterate over tokens
    pub fn tokens(&self) -> Box<dyn Iterator<Item = &Range<usize>> + '_> {
        match self {
            Self::Token(t) => Box::new(std::iter::once(t)),
            Self::Named { node, .. } => node.tokens(),
            Self::Unnamed(children) => Box::new(children.iter().flat_map(|n| n.tokens())),
            Self::Error(_) => Box::new(std::iter::empty()),
        }
    }

    /// Iterate over errors
    pub fn errors(&self) -> Box<dyn Iterator<Item = &Error> + '_> {
        match self {
            Self::Error(e) => Box::new(std::iter::once(e)),
            Self::Named { node, .. } => node.errors(),
            Self::Unnamed(children) => Box::new(children.iter().flat_map(|n| n.errors())),
            Self::Token(_) => Box::new(std::iter::empty()),
        }
    }

    /// Get range of whole syntax
    /// ```
    /// # use pretty_assertions::assert_eq;
    /// use dp::{syntax::Node, errors::Expected};
    ///
    /// let token = Node::Token(0..3);
    /// assert_eq!(token.range().unwrap(), 0..3);
    ///
    /// let unnamed: Node = vec![(0..1).into(), (1..3).into()].into();
    /// assert_eq!(unnamed.range().unwrap(), 0..3);
    ///
    /// let named = Node::Token(0..3).with_name("name");
    /// assert_eq!(named.range().unwrap(), 0..3);
    ///
    /// let err = Node::Error(
    /// 	Expected {
    /// 		expected: "x".to_string(),
    /// 		at: 0
    /// 	}.into()
    /// );
    /// assert_eq!(err.range(), None);
    /// ```
    pub fn range(&self) -> Option<Range<usize>> {
        let mut tokens = self.tokens();
        let first = tokens.next()?;

        let start = first.start;
        let end = tokens.last().unwrap_or(&first).end;
        Some(start..end)
    }

    /// Add or change name of this syntax
    /// ```
    /// # use pretty_assertions::assert_eq;
    /// use dp::{errors::Expected, syntax::Node};
    ///
    /// let token = Node::Token(0..0);
    /// assert_eq!(
    /// 	token.with_name("name"),
    /// 	Node::Named {
    /// 		name: "name".to_string(),
    /// 		node: Box::new((0..0).into())
    /// 	}
    /// );
    ///
    /// let unnamed: Node = vec![(0..1).into(), (1..2).into()].into();
    /// assert_eq!(
    /// 	unnamed.with_name("name"),
    /// 	Node::Named {
    /// 		name: "name".to_string(),
    /// 		node: Box::new(vec![(0..1).into(), (1..2).into()].into())
    /// 	}
    /// );
    ///
    /// let named = Node::Token(0..0).with_name("name");
    /// assert_eq!(
    /// 	named,
    /// 	Node::Named {
    /// 		name: "name".to_string(),
    /// 		node: Box::new((0..0).into())
    /// 	}
    /// );
    ///
    /// let err: Node =
    /// 	Expected {
    /// 		expected: "x".to_string(),
    /// 		at: 0
    /// 	}.into();
    /// assert_eq!(
    /// 	err.with_name("name"),
    /// 	Node::Named {
    /// 		name: "name".to_string(),
    /// 		node: Box::new(
    /// 			Expected {
    /// 				expected: "x".to_string(),
    /// 				at: 0
    /// 			}.into()
    /// 		)
    /// 	}
    /// );
    /// ```
    pub fn with_name(self, name: impl Into<String>) -> Self {
        let name = name.into();
        match self {
            Self::Named { node, .. } => Self::Named { name, node },
            _ => Self::Named {
                name,
                node: Box::new(self),
            },
        }
    }

    /// Check if this syntax has no errors
    pub fn is_ok(&self) -> bool {
        !self.has_errors()
    }

    /// Check if this syntax has errors
    pub fn has_errors(&self) -> bool {
        self.errors().next().is_some()
    }
}

impl<E: Into<Error>> From<E> for Node {
    fn from(value: E) -> Self {
        Self::Error(value.into())
    }
}
