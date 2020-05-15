use crate::syntax::{
    ast::{
        node::{BinOp, Block, Identifier, Node, VarDecl, VarDeclList},
        op::{AssignOp, CompOp, UnaryOp},
    },
    parser::tests::check_parser,
};

/// Checks do-while statement parsing.
#[test]
fn check_do_while() {
    check_parser(
        r#"do {
            a += 1;
        } while (true)"#,
        vec![Node::do_while_loop(
            Node::from(Block::from(vec![Node::from(BinOp::new(
                AssignOp::Add,
                Identifier::from("a"),
                Node::const_node(1),
            ))])),
            Node::const_node(true),
        )],
    );
}

// Checks automatic semicolon insertion after do-while.
#[test]
fn check_do_while_semicolon_insertion() {
    check_parser(
        r#"var i = 0;
        do {console.log("hello");} while(i++ < 10) console.log("end");"#,
        vec![
            VarDeclList::from(vec![VarDecl::new("i", Some(Node::const_node(0)))]).into(),
            Node::do_while_loop(
                Node::from(Block::from(vec![Node::call(
                    Node::get_const_field(Node::from(Identifier::from("console")), "log"),
                    vec![Node::const_node("hello")],
                )])),
                Node::from(BinOp::new(
                    CompOp::LessThan,
                    Node::unary_op(UnaryOp::IncrementPost, Node::from(Identifier::from("i"))),
                    Node::const_node(10),
                )),
            ),
            Node::call(
                Node::get_const_field(Node::from(Identifier::from("console")), "log"),
                vec![Node::const_node("end")],
            ),
        ],
    );
}
