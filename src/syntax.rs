use std::ops::Range;

use derive_more::From;

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
