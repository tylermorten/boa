use crate::syntax::{
    ast::node::{FormalParameter, FunctionExpr, MethodDefinitionKind, Node, PropertyDefinition},
    parser::tests::check_parser,
};

/// Checks object literal parsing.
#[test]
fn check_object_literal() {
    let object_properties = vec![
        PropertyDefinition::property("a", Node::const_node(true)),
        PropertyDefinition::property("b", Node::const_node(false)),
    ];

    check_parser(
        "const x = {
            a: true,
            b: false,
        };
        ",
        vec![Node::const_decl(vec![(
            "x".into(),
            Node::object(object_properties),
        )])],
    );
}

/// Tests short function syntax.
#[test]
fn check_object_short_function() {
    let object_properties = vec![
        PropertyDefinition::property("a", Node::const_node(true)),
        PropertyDefinition::method_definition(
            MethodDefinitionKind::Ordinary,
            "b",
            FunctionExpr::new(None, Vec::new(), Vec::new()),
        ),
    ];

    check_parser(
        "const x = {
            a: true,
            b() {},
        };
        ",
        vec![Node::const_decl(vec![(
            "x".into(),
            Node::object(object_properties),
        )])],
    );
}

/// Testing short function syntax with arguments.
#[test]
fn check_object_short_function_arguments() {
    let object_properties = vec![
        PropertyDefinition::property("a", Node::const_node(true)),
        PropertyDefinition::method_definition(
            MethodDefinitionKind::Ordinary,
            "b",
            FunctionExpr::new(
                None,
                vec![FormalParameter::new("test", None, false)],
                Vec::new(),
            ),
        ),
    ];

    check_parser(
        "const x = {
            a: true,
            b(test) {}
         };
        ",
        vec![Node::const_decl(vec![(
            "x".into(),
            Node::object(object_properties),
        )])],
    );
}

#[test]
fn check_object_getter() {
    let object_properties = vec![
        PropertyDefinition::property("a", Node::const_node(true)),
        PropertyDefinition::method_definition(
            MethodDefinitionKind::Get,
            "b",
            FunctionExpr::new(None, Vec::new(), Vec::new()),
        ),
    ];

    check_parser(
        "const x = {
            a: true,
            get b() {}
        };
        ",
        vec![Node::const_decl(vec![(
            "x".into(),
            Node::object(object_properties),
        )])],
    );
}

#[test]
fn check_object_setter() {
    let object_properties = vec![
        PropertyDefinition::property("a", Node::const_node(true)),
        PropertyDefinition::method_definition(
            MethodDefinitionKind::Set,
            "b",
            FunctionExpr::new(
                None,
                vec![FormalParameter::new("test", None, false)],
                Vec::new(),
            ),
        ),
    ];

    check_parser(
        "const x = {
            a: true,
            set b(test) {}
        };
        ",
        vec![Node::const_decl(vec![(
            "x".into(),
            Node::object(object_properties),
        )])],
    );
}
