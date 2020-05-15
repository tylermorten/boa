use crate::syntax::{
    ast::{
        node::{Block, Identifier, Node, VarDecl, VarDeclList},
        Const,
    },
    parser::tests::{check_invalid, check_parser},
};

#[test]
fn check_inline_with_empty_try_catch() {
    check_parser(
        "try { } catch(e) {}",
        vec![Node::try_node(
            Block::from(vec![]),
            Some((Some(Identifier::from("e")), Block::from(vec![]))),
            None,
        )],
    );
}

#[test]
fn check_inline_with_var_decl_inside_try() {
    check_parser(
        "try { var x = 1; } catch(e) {}",
        vec![Node::try_node(
            Block::from(vec![VarDeclList::from(vec![VarDecl::new(
                "x",
                Some(Const::from(1).into()),
            )])
            .into()]),
            Some((Some(Identifier::from("e")), Block::from(vec![]))),
            None,
        )],
    );
}

#[test]
fn check_inline_with_var_decl_inside_catch() {
    check_parser(
        "try { var x = 1; } catch(e) { var x = 1; }",
        vec![Node::try_node(
            Block::from(vec![VarDeclList::from(vec![VarDecl::new(
                "x",
                Some(Const::from(1).into()),
            )])
            .into()]),
            Some((
                Some(Identifier::from("e")),
                Block::from(vec![VarDeclList::from(vec![VarDecl::new(
                    "x",
                    Some(Const::from(1).into()),
                )])
                .into()]),
            )),
            None,
        )],
    );
}

#[test]
fn check_inline_with_empty_try_catch_finally() {
    check_parser(
        "try {} catch(e) {} finally {}",
        vec![Node::try_node(
            Block::from(vec![]),
            Some((Some(Identifier::from("e")), Block::from(vec![]))),
            Block::from(vec![]),
        )],
    );
}

#[test]
fn check_inline_with_empty_try_finally() {
    check_parser(
        "try {} finally {}",
        vec![Node::try_node(
            Block::from(vec![]),
            None,
            Block::from(vec![]),
        )],
    );
}

#[test]
fn check_inline_with_empty_try_var_decl_in_finally() {
    check_parser(
        "try {} finally { var x = 1; }",
        vec![Node::try_node(
            Block::from(vec![]),
            None,
            Block::from(vec![VarDeclList::from(vec![VarDecl::new(
                "x",
                Some(Const::from(1).into()),
            )])
            .into()]),
        )],
    );
}

#[test]
fn check_inline_empty_try_paramless_catch() {
    check_parser(
        "try {} catch { var x = 1; }",
        vec![Node::try_node(
            Block::from(vec![]),
            Some((
                None,
                Block::from(vec![VarDeclList::from(vec![VarDecl::new(
                    "x",
                    Some(Const::from(1).into()),
                )])
                .into()]),
            )),
            None,
        )],
    );
}

#[test]
fn check_inline_invalid_catch() {
    check_invalid("try {} catch");
}

#[test]
fn check_inline_invalid_catch_without_closing_paren() {
    check_invalid("try {} catch(e {}");
}

#[test]
fn check_inline_invalid_catch_parameter() {
    check_invalid("try {} catch(1) {}");
}
