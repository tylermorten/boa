//! Tests for the parser.

use super::Parser;
use crate::syntax::{
    ast::{
        node::{
            Assign, BinOp, FunctionDecl, Identifier, Node, StatementList, VarDecl, VarDeclList,
        },
        op::{NumOp, UnaryOp},
        Const,
    },
    lexer::Lexer,
};

/// Checks that the given JavaScript string gives the expected expression.
#[allow(clippy::result_unwrap_used)]
// TODO: #[track_caller]: https://github.com/rust-lang/rust/issues/47809
pub(super) fn check_parser<L>(js: &str, expr: L)
where
    L: Into<Box<[Node]>>,
{
    let mut lexer = Lexer::new(js);
    lexer.lex().expect("failed to lex");

    assert_eq!(
        Parser::new(&lexer.tokens)
            .parse_all()
            .expect("failed to parse"),
        StatementList::from(expr)
    );
}

/// Checks that the given javascript string creates a parse error.
// TODO: #[track_caller]: https://github.com/rust-lang/rust/issues/47809
pub(super) fn check_invalid(js: &str) {
    let mut lexer = Lexer::new(js);
    lexer.lex().expect("failed to lex");

    assert!(Parser::new(&lexer.tokens).parse_all().is_err());
}

/// Should be parsed as `new Class().method()` instead of `new (Class().method())`
#[test]
fn check_construct_call_precedence() {
    check_parser(
        "new Date().getTime()",
        vec![Node::call(
            Node::get_const_field(
                Node::new(Node::call(Node::from(Identifier::from("Date")), Vec::new())),
                "getTime",
            ),
            Vec::new(),
        )],
    );
}

#[test]
fn assign_operator_precedence() {
    check_parser(
        "a = a + 1",
        vec![Assign::new(
            Identifier::from("a"),
            BinOp::new(NumOp::Add, Identifier::from("a"), Const::from(1)),
        )
        .into()],
    );
}

#[test]
fn hoisting() {
    check_parser(
        r"
            var a = hello();
            a++;

            function hello() { return 10 }",
        vec![
            FunctionDecl::new(
                Box::from("hello"),
                vec![],
                vec![Node::return_node(Const::from(10))],
            )
            .into(),
            VarDeclList::from(vec![VarDecl::new(
                "a",
                Some(Node::call(Node::from(Identifier::from("hello")), vec![])),
            )])
            .into(),
            Node::unary_op(UnaryOp::IncrementPost, Node::from(Identifier::from("a"))),
        ],
    );

    check_parser(
        r"
            a = 10;
            a++;

            var a;",
        vec![
            Node::from(Assign::new(Identifier::from("a"), Const::from(10))),
            Node::unary_op(UnaryOp::IncrementPost, Node::from(Identifier::from("a"))),
            VarDeclList::from(vec![VarDecl::new("a", None)]).into(),
        ],
    );
}
