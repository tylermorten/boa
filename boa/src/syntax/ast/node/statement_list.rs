//! Statement list node.

use super::Node;
use gc::{Finalize, Trace};
use std::fmt;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// List of statements.
///
/// Similar to `Node::Block` but without the braces.
///
/// More information:
///  - [ECMAScript reference][spec]
///
/// [spec]: https://tc39.es/ecma262/#prod-StatementList
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, Trace, Finalize, PartialEq)]
pub struct StatementList {
    #[cfg_attr(feature = "serde", serde(flatten))]
    statements: Box<[Node]>,
}

impl StatementList {
    /// Gets the list of statements.
    pub fn statements(&self) -> &[Node] {
        &self.statements
    }

    // /// Gets the lexically declared names.
    // ///
    // /// More information:
    // /// <https://tc39.es/ecma262/#sec-block-static-semantics-lexicallydeclarednames>
    // pub(crate) fn lexically_declared_names(&self) -> &[Box<str>] {
    //     static LIST: OnceCell<Box<[Box<str>]>> = OnceCell::new();

    //     LIST.get_or_init(|| {
    //         self.statements
    //             .iter()
    //             .map(|node| node.lexically_declared_names())
    //             .flatten()
    //             .cloned()
    //             .collect::<Vec<_>>()
    //             .into_boxed_slice()
    //     })
    // }

    /// Implements the display formatting with indentation.
    pub(super) fn display(&self, f: &mut fmt::Formatter<'_>, indentation: usize) -> fmt::Result {
        // Print statements
        for node in self.statements.iter() {
            node.display(f, indentation + 1)?;

            match node {
                Node::Block(_)
                | Node::If(_, _, _)
                | Node::Switch(_, _, _)
                | Node::WhileLoop(_, _) => {}
                _ => write!(f, ";")?,
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<T> From<T> for StatementList
where
    T: Into<Box<[Node]>>,
{
    fn from(stm: T) -> Self {
        Self {
            statements: stm.into(),
        }
    }
}

impl fmt::Display for StatementList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(f, 0)
    }
}
