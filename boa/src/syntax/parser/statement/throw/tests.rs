use crate::syntax::{ast::Node, parser::tests::check_parser};

#[test]
fn check_throw_parsing() {
    check_parser(
        "throw 'error';",
        vec![Node::throw(Node::const_node("error"))],
    );
}
