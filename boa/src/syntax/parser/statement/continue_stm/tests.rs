use crate::syntax::{
    ast::node::{Block, Node},
    parser::tests::check_parser,
};

#[test]
fn inline() {
    check_parser(
        "while (true) continue;",
        vec![Node::while_loop(
            Node::const_node(true),
            Node::Continue(None),
        )],
    );
}

#[test]
fn new_line() {
    check_parser(
        "while (true)
            continue;",
        vec![Node::while_loop(
            Node::const_node(true),
            Node::Continue(None),
        )],
    );
}

#[test]
fn inline_block_semicolon_insertion() {
    check_parser(
        "while (true) {continue}",
        vec![Node::while_loop(
            Node::const_node(true),
            Node::from(Block::from(vec![Node::Continue(None)])),
        )],
    );
}

#[test]
fn new_line_semicolon_insertion() {
    check_parser(
        "while (true) {
            continue test
        }",
        vec![Node::while_loop(
            Node::const_node(true),
            Node::from(Block::from(vec![Node::continue_node("test")])),
        )],
    );
}

#[test]
fn inline_block() {
    check_parser(
        "while (true) {continue;}",
        vec![Node::while_loop(
            Node::const_node(true),
            Node::from(Block::from(vec![Node::Continue(None)])),
        )],
    );
}

#[test]
fn new_line_block() {
    check_parser(
        "while (true) {
            continue test;
        }",
        vec![Node::while_loop(
            Node::const_node(true),
            Node::from(Block::from(vec![Node::continue_node("test")])),
        )],
    );
}

#[test]
fn reserved_label() {
    check_parser(
        "while (true) {
            continue await;
        }",
        vec![Node::while_loop(
            Node::const_node(true),
            Node::from(Block::from(vec![Node::continue_node("await")])),
        )],
    );

    check_parser(
        "while (true) {
            continue yield;
        }",
        vec![Node::while_loop(
            Node::const_node(true),
            Node::from(Block::from(vec![Node::continue_node("yield")])),
        )],
    );
}

#[test]
fn new_line_block_empty() {
    check_parser(
        "while (true) {
            continue;
        }",
        vec![Node::while_loop(
            Node::const_node(true),
            Node::from(Block::from(vec![Node::Continue(None)])),
        )],
    );
}

#[test]
fn new_line_block_empty_semicolon_insertion() {
    check_parser(
        "while (true) {
            continue
        }",
        vec![Node::while_loop(
            Node::const_node(true),
            Node::from(Block::from(vec![Node::Continue(None)])),
        )],
    );
}
