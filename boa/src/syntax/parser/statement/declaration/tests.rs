use crate::syntax::{
    ast::node::{FunctionDecl, Node, VarDecl, VarDeclList},
    parser::tests::{check_invalid, check_parser},
};

/// Checks `var` declaration parsing.
#[test]
fn var_declaration() {
    check_parser(
        "var a = 5;",
        vec![VarDeclList::from(vec![VarDecl::new("a", Some(Node::const_node(5)))]).into()],
    );
}

/// Checks `var` declaration parsing with reserved words.
#[test]
fn var_declaration_keywords() {
    check_parser(
        "var yield = 5;",
        vec![VarDeclList::from(vec![VarDecl::new("yield", Some(Node::const_node(5)))]).into()],
    );

    check_parser(
        "var await = 5;",
        vec![VarDeclList::from(vec![VarDecl::new("await", Some(Node::const_node(5)))]).into()],
    );
}

/// Checks `var` declaration parsing with no spaces.
#[test]
fn var_declaration_no_spaces() {
    check_parser(
        "var a=5;",
        vec![VarDeclList::from(vec![VarDecl::new("a", Some(Node::const_node(5)))]).into()],
    );
}

/// Checks empty `var` declaration parsing.
#[test]
fn empty_var_declaration() {
    check_parser(
        "var a;",
        vec![VarDeclList::from(vec![VarDecl::new("a", None)]).into()],
    );
}

/// Checks multiple `var` declarations.
#[test]
fn multiple_var_declaration() {
    check_parser(
        "var a = 5, b, c = 6;",
        vec![VarDeclList::from(vec![
            VarDecl::new("a", Some(Node::const_node(5))),
            VarDecl::new("b", None),
            VarDecl::new("c", Some(Node::const_node(6))),
        ])
        .into()],
    );
}

/// Checks `let` declaration parsing.
#[test]
fn let_declaration() {
    check_parser(
        "let a = 5;",
        vec![Node::let_decl(vec![(
            "a".into(),
            Some(Node::const_node(5)),
        )])],
    );
}

/// Checks `let` declaration parsing with reserved words.
#[test]
fn let_declaration_keywords() {
    check_parser(
        "let yield = 5;",
        vec![Node::let_decl(vec![(
            "yield".into(),
            Some(Node::const_node(5)),
        )])],
    );

    check_parser(
        "let await = 5;",
        vec![Node::let_decl(vec![(
            "await".into(),
            Some(Node::const_node(5)),
        )])],
    );
}

/// Checks `let` declaration parsing with no spaces.
#[test]
fn let_declaration_no_spaces() {
    check_parser(
        "let a=5;",
        vec![Node::let_decl(vec![(
            "a".into(),
            Some(Node::const_node(5)),
        )])],
    );
}

/// Checks empty `let` declaration parsing.
#[test]
fn empty_let_declaration() {
    check_parser("let a;", vec![Node::let_decl(vec![("a".into(), None)])]);
}

/// Checks multiple `let` declarations.
#[test]
fn multiple_let_declaration() {
    check_parser(
        "let a = 5, b, c = 6;",
        vec![Node::let_decl(vec![
            ("a".into(), Some(Node::const_node(5))),
            ("b".into(), None),
            ("c".into(), Some(Node::const_node(6))),
        ])],
    );
}

/// Checks `const` declaration parsing.
#[test]
fn const_declaration() {
    check_parser(
        "const a = 5;",
        vec![Node::const_decl(vec![("a".into(), Node::const_node(5))])],
    );
}

/// Checks `const` declaration parsing with reserved words.
#[test]
fn const_declaration_keywords() {
    check_parser(
        "const yield = 5;",
        vec![Node::const_decl(vec![(
            "yield".into(),
            Node::const_node(5),
        )])],
    );

    check_parser(
        "const await = 5;",
        vec![Node::const_decl(vec![(
            "await".into(),
            Node::const_node(5),
        )])],
    );
}

/// Checks `const` declaration parsing with no spaces.
#[test]
fn const_declaration_no_spaces() {
    check_parser(
        "const a=5;",
        vec![Node::const_decl(vec![("a".into(), Node::const_node(5))])],
    );
}

/// Checks empty `const` declaration parsing.
#[test]
fn empty_const_declaration() {
    check_invalid("const a;");
}

/// Checks multiple `const` declarations.
#[test]
fn multiple_const_declaration() {
    check_parser(
        "const a = 5, c = 6;",
        vec![Node::const_decl(vec![
            ("a".into(), Node::const_node(5)),
            ("c".into(), Node::const_node(6)),
        ])],
    );
}

/// Function declaration parsing.
#[test]
fn function_declaration() {
    check_parser(
        "function hello() {}",
        vec![FunctionDecl::new(Box::from("hello"), vec![], vec![]).into()],
    );
}

/// Function declaration parsing with keywords.
#[test]
fn function_declaration_keywords() {
    check_parser(
        "function yield() {}",
        vec![FunctionDecl::new(Box::from("yield"), vec![], vec![]).into()],
    );

    check_parser(
        "function await() {}",
        vec![FunctionDecl::new(Box::from("await"), vec![], vec![]).into()],
    );
}
