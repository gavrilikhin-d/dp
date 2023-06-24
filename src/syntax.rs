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
}

impl Node {
    /// Iterate over tokens
    pub fn tokens(&self) -> Box<dyn Iterator<Item = &Range<usize>> + '_> {
        match self {
            Node::Token(t) => Box::new(std::iter::once(t)),
            Node::Named { node, .. } => node.tokens(),
            Node::Unnamed(children) => Box::new(children.iter().flat_map(|n| n.tokens())),
        }
    }

    /// Get range of whole syntax
    /// ```
    /// # use pretty_assertions::assert_eq;
    /// use dp::syntax::Node;
    ///
    /// let token = Node::Token(0..3);
    /// assert_eq!(token.range().unwrap(), 0..3);
    ///
    /// let unnamed: Node = vec![(0..1).into(), (1..3).into()].into();
    /// assert_eq!(unnamed.range().unwrap(), 0..3);
    ///
    /// let named = Node::Token(0..3).with_name("name");
    /// assert_eq!(named.range().unwrap(), 0..3);
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
    /// use dp::syntax::Node;
    ///
    /// let token = Node::Token(0..0);
    /// assert_eq!(
    /// 	token.with_name("name"),
    /// 	Node::Named {
    /// 		name: "name".to_string(),
    /// 		node: Box::new(Node::Token(0..0))
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
    /// 		node: Box::new(Node::Token(0..0))
    /// 	}
    /// );
    /// ```
    pub fn with_name(self, name: impl Into<String>) -> Self {
        let name = name.into();
        match self {
            Node::Named { node, .. } => Node::Named { name, node },
            _ => Node::Named {
                name,
                node: Box::new(self),
            },
        }
    }
}

/// Syntax tree node
#[derive(Debug, Clone, PartialEq, Eq, From)]
pub enum NodeWithErrors {
    /// Named node in the syntax tree
    Named {
        name: String,
        node: Box<NodeWithErrors>,
    },
    /// Token in the syntax tree
    Unnamed(Vec<NodeWithErrors>),
    /// Leaf token in the syntax tree
    Token(Range<usize>),
    /// Error in the syntax tree
    #[from(ignore)]
    Error(Error),
}

impl NodeWithErrors {
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
    /// use dp::{syntax::NodeWithErrors, errors::Expected};
    ///
    /// let token = NodeWithErrors::Token(0..3);
    /// assert_eq!(token.range().unwrap(), 0..3);
    ///
    /// let unnamed: NodeWithErrors = vec![(0..1).into(), (1..3).into()].into();
    /// assert_eq!(unnamed.range().unwrap(), 0..3);
    ///
    /// let named = NodeWithErrors::Token(0..3).with_name("name");
    /// assert_eq!(named.range().unwrap(), 0..3);
    ///
    /// let err = NodeWithErrors::Error(
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
    /// use dp::{errors::Expected, syntax::NodeWithErrors};
    ///
    /// let token = NodeWithErrors::Token(0..0);
    /// assert_eq!(
    /// 	token.with_name("name"),
    /// 	NodeWithErrors::Named {
    /// 		name: "name".to_string(),
    /// 		node: Box::new((0..0).into())
    /// 	}
    /// );
    ///
    /// let unnamed: NodeWithErrors = vec![(0..1).into(), (1..2).into()].into();
    /// assert_eq!(
    /// 	unnamed.with_name("name"),
    /// 	NodeWithErrors::Named {
    /// 		name: "name".to_string(),
    /// 		node: Box::new(vec![(0..1).into(), (1..2).into()].into())
    /// 	}
    /// );
    ///
    /// let named = NodeWithErrors::Token(0..0).with_name("name");
    /// assert_eq!(
    /// 	named,
    /// 	NodeWithErrors::Named {
    /// 		name: "name".to_string(),
    /// 		node: Box::new((0..0).into())
    /// 	}
    /// );
    ///
    /// let err: NodeWithErrors =
    /// 	Expected {
    /// 		expected: "x".to_string(),
    /// 		at: 0
    /// 	}.into();
    /// assert_eq!(
    /// 	err.with_name("name"),
    /// 	NodeWithErrors::Named {
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
}

impl<E: Into<Error>> From<E> for NodeWithErrors {
    fn from(value: E) -> Self {
        Self::Error(value.into())
    }
}

impl From<Node> for NodeWithErrors {
    fn from(node: Node) -> Self {
        match node {
            Node::Named { name, node } => Self::Named {
                name,
                node: Box::new((*node).into()),
            },
            Node::Unnamed(children) => Self::Unnamed(
                children
                    .into_iter()
                    .map(|n| n.into())
                    .collect::<Vec<_>>()
                    .into(),
            ),
            Node::Token(t) => Self::Token(t),
        }
    }
}

impl TryFrom<NodeWithErrors> for Node {
    type Error = Vec<Error>;

    fn try_from(node: NodeWithErrors) -> Result<Self, Self::Error> {
        let errs = node.errors().cloned().collect::<Vec<_>>();
        if !errs.is_empty() {
            return Err(errs);
        }

        Ok(match node {
            NodeWithErrors::Named { name, node } => Self::Named {
                name,
                node: Box::new((*node).try_into().unwrap()),
            },
            NodeWithErrors::Unnamed(children) => Self::Unnamed(
                children
                    .into_iter()
                    .map(|n| n.try_into())
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap(),
            ),
            NodeWithErrors::Token(t) => Self::Token(t),
            NodeWithErrors::Error(_) => unreachable!(),
        })
    }
}
