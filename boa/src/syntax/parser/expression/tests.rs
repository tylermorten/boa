use crate::syntax::{
    ast::node::{BinOp, Identifier, Node},
    ast::op::{AssignOp, BitOp, CompOp, NumOp},
    parser::tests::check_parser,
};

/// Checks numeric operations
#[test]
fn check_numeric_operations() {
    check_parser(
        "a + b",
        vec![Node::from(BinOp::new(
            NumOp::Add,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a+1",
        vec![Node::from(BinOp::new(
            NumOp::Add,
            Identifier::from("a"),
            Node::const_node(1),
        ))],
    );
    check_parser(
        "a - b",
        vec![Node::from(BinOp::new(
            NumOp::Sub,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a-1",
        vec![Node::from(BinOp::new(
            NumOp::Sub,
            Identifier::from("a"),
            Node::const_node(1),
        ))],
    );
    check_parser(
        "a / b",
        vec![Node::from(BinOp::new(
            NumOp::Div,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a/2",
        vec![Node::from(BinOp::new(
            NumOp::Div,
            Identifier::from("a"),
            Node::const_node(2),
        ))],
    );
    check_parser(
        "a * b",
        vec![Node::from(BinOp::new(
            NumOp::Mul,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a*2",
        vec![Node::from(BinOp::new(
            NumOp::Mul,
            Identifier::from("a"),
            Node::const_node(2),
        ))],
    );
    check_parser(
        "a ** b",
        vec![Node::from(BinOp::new(
            NumOp::Exp,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a**2",
        vec![Node::from(BinOp::new(
            NumOp::Exp,
            Identifier::from("a"),
            Node::const_node(2),
        ))],
    );
    check_parser(
        "a % b",
        vec![Node::from(BinOp::new(
            NumOp::Mod,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a%2",
        vec![Node::from(BinOp::new(
            NumOp::Mod,
            Identifier::from("a"),
            Node::const_node(2),
        ))],
    );
}

// Checks complex numeric operations.
#[test]
fn check_complex_numeric_operations() {
    check_parser(
        "a + d*(b-3)+1",
        vec![Node::from(BinOp::new(
            NumOp::Add,
            Node::from(BinOp::new(
                NumOp::Add,
                Identifier::from("a"),
                Node::from(BinOp::new(
                    NumOp::Mul,
                    Identifier::from("d"),
                    Node::from(BinOp::new(
                        NumOp::Sub,
                        Identifier::from("b"),
                        Node::const_node(3),
                    )),
                )),
            )),
            Node::const_node(1),
        ))],
    );
}

/// Checks bitwise operations.
#[test]
fn check_bitwise_operations() {
    check_parser(
        "a & b",
        vec![Node::from(BinOp::new(
            BitOp::And,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a&b",
        vec![Node::from(BinOp::new(
            BitOp::And,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );

    check_parser(
        "a | b",
        vec![Node::from(BinOp::new(
            BitOp::Or,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a|b",
        vec![Node::from(BinOp::new(
            BitOp::Or,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );

    check_parser(
        "a ^ b",
        vec![Node::from(BinOp::new(
            BitOp::Xor,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a^b",
        vec![Node::from(BinOp::new(
            BitOp::Xor,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );

    check_parser(
        "a << b",
        vec![Node::from(BinOp::new(
            BitOp::Shl,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a<<b",
        vec![Node::from(BinOp::new(
            BitOp::Shl,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );

    check_parser(
        "a >> b",
        vec![Node::from(BinOp::new(
            BitOp::Shr,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a>>b",
        vec![Node::from(BinOp::new(
            BitOp::Shr,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
}

/// Checks assignment operations.
#[test]
fn check_assign_operations() {
    check_parser(
        "a += b",
        vec![Node::from(BinOp::new(
            AssignOp::Add,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a -= b",
        vec![Node::from(BinOp::new(
            AssignOp::Sub,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a *= b",
        vec![Node::from(BinOp::new(
            AssignOp::Mul,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a **= b",
        vec![Node::from(BinOp::new(
            AssignOp::Exp,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a /= b",
        vec![Node::from(BinOp::new(
            AssignOp::Div,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a %= b",
        vec![Node::from(BinOp::new(
            AssignOp::Mod,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a &= b",
        vec![Node::from(BinOp::new(
            AssignOp::And,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a |= b",
        vec![Node::from(BinOp::new(
            AssignOp::Or,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a ^= b",
        vec![Node::from(BinOp::new(
            AssignOp::Xor,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a <<= b",
        vec![Node::from(BinOp::new(
            AssignOp::Shl,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a >>= b",
        vec![Node::from(BinOp::new(
            AssignOp::Shr,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a %= 10 / 2",
        vec![Node::from(BinOp::new(
            AssignOp::Mod,
            Identifier::from("a"),
            Node::from(BinOp::new(
                NumOp::Div,
                Node::const_node(10),
                Node::const_node(2),
            )),
        ))],
    );
}

#[test]
fn check_relational_operations() {
    check_parser(
        "a < b",
        vec![Node::from(BinOp::new(
            CompOp::LessThan,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a > b",
        vec![Node::from(BinOp::new(
            CompOp::GreaterThan,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a <= b",
        vec![Node::from(BinOp::new(
            CompOp::LessThanOrEqual,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "a >= b",
        vec![Node::from(BinOp::new(
            CompOp::GreaterThanOrEqual,
            Identifier::from("a"),
            Identifier::from("b"),
        ))],
    );
    check_parser(
        "p in o",
        vec![Node::from(BinOp::new(
            CompOp::In,
            Identifier::from("p"),
            Identifier::from("o"),
        ))],
    );
}
