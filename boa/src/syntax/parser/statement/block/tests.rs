//! Block statement parsing tests.

use crate::syntax::{
    ast::{
        node::{Assign, Block, FunctionDecl, Identifier, Node, VarDecl, VarDeclList},
        op::UnaryOp,
        Const,
    },
    parser::tests::check_parser,
};

/// Helper function to check a block.
// TODO: #[track_caller]: https://github.com/rust-lang/rust/issues/47809
fn check_block(js: &str, block: Block) {
    check_parser(js, vec![Node::from(block)]);
}

#[test]
fn empty() {
    check_block("{}", Block::from(vec![]));
}

#[test]
fn non_empty() {
    check_block(
        r"{
            var a = 10;
            a++;
        }",
        Block::from(vec![
            VarDeclList::from(vec![VarDecl::new("a", Some(Const::from(10).into()))]).into(),
            Node::unary_op(UnaryOp::IncrementPost, Node::from(Identifier::from("a"))),
        ]),
    );

    check_block(
        r"{
            function hello() {
                return 10
            }

            var a = hello();
            a++;
        }",
        Block::from(vec![
            FunctionDecl::new(
                "hello".to_owned().into_boxed_str(),
                vec![],
                vec![Node::return_node(Node::from(Const::from(10)))],
            )
            .into(),
            VarDeclList::from(vec![VarDecl::new(
                "a",
                Some(Node::call(Node::from(Identifier::from("hello")), vec![])),
            )])
            .into(),
            Node::unary_op(UnaryOp::IncrementPost, Node::from(Identifier::from("a"))),
        ]),
    );
}

#[test]
fn hoisting() {
    check_block(
        r"{
            var a = hello();
            a++;

            function hello() { return 10 }
        }",
        Block::from(vec![
            FunctionDecl::new(
                "hello".to_owned().into_boxed_str(),
                vec![],
                vec![Node::return_node(Node::from(Const::from(10)))],
            )
            .into(),
            VarDeclList::from(vec![VarDecl::new(
                "a",
                Some(Node::call(Node::from(Identifier::from("hello")), vec![])),
            )])
            .into(),
            Node::unary_op(UnaryOp::IncrementPost, Node::from(Identifier::from("a"))),
        ]),
    );

    check_block(
        r"{
            a = 10;
            a++;

            var a;
        }",
        Block::from(vec![
            Assign::new(Identifier::from("a"), Const::from(10)).into(),
            Node::unary_op(UnaryOp::IncrementPost, Node::from(Identifier::from("a"))),
            VarDeclList::from(vec![VarDecl::new("a", None)]).into(),
        ]),
    );
}
