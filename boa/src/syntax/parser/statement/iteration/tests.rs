use crate::syntax::{
    ast::{
        node::{BinOp, Block, Identifier, Node, VarDecl, VarDeclList},
        op::{AssignOp, CompOp, UnaryOp},
        Const,
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
            Block::from(vec![Node::from(BinOp::new(
                AssignOp::Add,
                Identifier::from("a"),
                Const::from(1),
            ))]),
            Const::from(true),
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
            VarDeclList::from(vec![VarDecl::new("i", Some(Const::from(0).into()))]).into(),
            Node::do_while_loop(
                Block::from(vec![Node::call(
                    Node::get_const_field(Identifier::from("console"), "log"),
                    vec![Const::from("hello").into()],
                )]),
                BinOp::new(
                    CompOp::LessThan,
                    Node::unary_op(UnaryOp::IncrementPost, Identifier::from("i")),
                    Const::from(10),
                ),
            ),
            Node::call(
                Node::get_const_field(Identifier::from("console"), "log"),
                vec![Const::from("end").into()],
            ),
        ],
    );
}
