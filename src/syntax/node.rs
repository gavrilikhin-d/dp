use std::ops::{Index, IndexMut, Range};

use derive_more::From;

use crate::errors::Error;

use super::{token, Token};

/// Syntax tree node
#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum Node {
    /// Named node in the syntax tree
    Named { name: String, node: Box<Node> },
    /// Token in the syntax tree
    #[from]
    Unnamed(Vec<Node>),
    /// Leaf token in the syntax tree
    Token(Token),
    /// Error in the syntax tree
    Error(Error),
}

impl Node {
    /// Iterate over tokens
    pub fn tokens(&self) -> Box<dyn Iterator<Item = &Token> + '_> {
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
    /// use dp::{syntax::{Node, Token, token::Kind}, errors::Expected};
    ///
    /// let token: Node = (Kind::Keyword, 0..3).into();
    /// assert_eq!(token.range().unwrap(), 0..3);
    ///
    /// let unnamed: Node = vec![
    /// 	(Kind::Keyword, 0..1).into(),
    /// 	(Kind::Keyword, 1..3).into()
    /// ].into();
    /// assert_eq!(unnamed.range().unwrap(), 0..3);
    ///
    /// let named = Node::from((Kind::Keyword, 0..3)).with_name("name");
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

        let start = first.range.start;
        let end = tokens.last().unwrap_or(&first).range.end;
        Some(start..end)
    }

    /// Get length of syntax
    pub fn len(&self) -> usize {
        self.range().map_or(0, |r| r.len())
    }

    /// Add or change name of this syntax
    /// ```
    /// # use pretty_assertions::assert_eq;
    /// use dp::{errors::Expected, syntax::{Node, token::Kind}};
    ///
    /// let token: Node = (Kind::Keyword, 0..0).into();
    /// assert_eq!(
    /// 	token.with_name("name"),
    /// 	Node::Named {
    /// 		name: "name".to_string(),
    /// 		node: Box::new((Kind::Keyword, 0..0).into())
    /// 	}
    /// );
    ///
    /// let unnamed: Node = vec![
    /// 	(Kind::Keyword, 0..1).into(),
    /// 	(Kind::Keyword, 1..2).into()
    /// ].into();
    /// assert_eq!(
    /// 	unnamed.with_name("name"),
    /// 	Node::Named {
    /// 		name: "name".to_string(),
    /// 		node: Box::new(
    /// 			vec![
    /// 				(Kind::Keyword, 0..1).into(),
    /// 				(Kind::Keyword, 1..2).into()
    /// 			].into()
    /// 		)
    /// 	}
    /// );
    ///
    /// let named = Node::from((Kind::Keyword, 0..0)).with_name("name");
    /// assert_eq!(
    /// 	named,
    /// 	Node::Named {
    /// 		name: "name".to_string(),
    /// 		node: Box::new((Kind::Keyword, 0..0).into())
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
            Self::Unnamed(children) if children.len() == 1 => Self::Named {
                name,
                node: Box::new(children.into_iter().next().unwrap()),
            },
            _ => Self::Named {
                name,
                node: Box::new(self),
            },
        }
    }

    /// Create named node
    pub fn named(name: impl Into<String>, node: impl Into<Node>) -> Self {
        Self::Named {
            name: name.into(),
            node: Box::new(node.into()),
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

    /// Unwrap this node as token
    pub fn as_token_mut(&mut self) -> &mut Token {
        match self {
            Self::Token(token) => token,
            _ => panic!("node is not a token"),
        }
    }

    /// Get name of this node, if any
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Named { name, .. } => Some(name),
            _ => None,
        }
    }
}

impl<E: Into<Error>> From<E> for Node {
    fn from(value: E) -> Self {
        Self::Error(value.into())
    }
}

impl From<(token::Kind, Range<usize>)> for Node {
    fn from(value: (token::Kind, Range<usize>)) -> Self {
        Self::Token(value.into())
    }
}

impl Index<usize> for Node {
    type Output = Node;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Self::Named { node, .. } => &node[index],
            Self::Unnamed(children) => &children[index],
            Self::Token(_) => panic!("Cannot index token"),
            Self::Error(_) => panic!("Cannot index error"),
        }
    }
}

impl IndexMut<usize> for Node {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match self {
            Self::Named { node, .. } => &mut node[index],
            Self::Unnamed(children) => &mut children[index],
            Self::Token(_) => panic!("Cannot index token"),
            Self::Error(_) => panic!("Cannot index error"),
        }
    }
}

impl Index<&str> for Node {
    type Output = Node;

    fn index(&self, index: &str) -> &Self::Output {
        match self {
            Self::Named { node, .. } => &node[index],
            Self::Unnamed(children) => &children
                .iter()
                .find(|c| c.name() == Some(index))
                .map(|n| match n {
                    Node::Named { node, .. } => node,
                    _ => unreachable!(),
                })
                .unwrap(),
            Self::Token(_) => panic!("Cannot index token"),
            Self::Error(_) => panic!("Cannot index error"),
        }
    }
}

impl IndexMut<&str> for Node {
    fn index_mut(&mut self, index: &str) -> &mut Self::Output {
        match self {
            Self::Named { node, .. } => &mut node[index],
            Self::Unnamed(children) => children
                .iter_mut()
                .find(|c| c.name() == Some(index))
                .map(|n| match n {
                    Node::Named { node, .. } => node,
                    _ => unreachable!(),
                })
                .unwrap(),
            Self::Token(_) => panic!("Cannot index token"),
            Self::Error(_) => panic!("Cannot index error"),
        }
    }
}
