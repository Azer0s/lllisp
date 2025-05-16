#[cfg(test)]
mod macro_direct_ast_tests {
    use lllisp::ast::{ExprKind, Literal, Located, Span, Program, TopLevel, TopLevelKind};
    use lllisp::macro_expander::MacroExpander;
    use lllisp::interpreter::{Interpreter, Value};

    // Test constructing and expanding an identity macro directly through AST construction
    #[test]
    fn test_identity_macro_direct_ast() {
        // Manually create an identity macro
        let macro_def = TopLevel {
            node: TopLevelKind::MacroDef {
                name: "identity".to_string(),
                params: vec!["x".to_string()],
                body: Located::new(
                    ExprKind::Symbol("x".to_string()),
                    Span::new(0, 0),
                ),
            },
            span: Span::new(0, 0),
        };

        // Create a variable definition using the macro
        let var_def = TopLevel {
            node: TopLevelKind::VarDef {
                name: "id-result".to_string(),
                value: Located::new(
                    ExprKind::Call {
                        name: "identity".to_string(),
                        args: vec![
                            Located::new(
                                ExprKind::Literal(Literal::Integer(42)),
                                Span::new(0, 0),
                            ),
                        ],
                    },
                    Span::new(0, 0),
                ),
            },
            span: Span::new(0, 0),
        };

        // Create a program with the macro definition and variable definition
        let program = Program {
            forms: vec![macro_def, var_def],
        };

        // Process the program with the macro expander
        let mut expander = MacroExpander::new();
        let expanded_program = expander.process_program(&program);

        // Check that the id-result is now 42 (expanded)
        assert_eq!(expanded_program.forms.len(), 1);
        match &expanded_program.forms[0].node {
            TopLevelKind::VarDef { name, value } => {
                assert_eq!(name, "id-result");
                match &value.node {
                    ExprKind::Literal(Literal::Integer(val)) => {
                        assert_eq!(*val, 42);
                    },
                    _ => panic!("Expected Integer literal, got {:?}", value.node),
                }
            },
            _ => panic!("Expected VarDef, got {:?}", expanded_program.forms[0].node),
        }
    }

    // Test constructing and expanding a double macro directly through AST construction
    #[test]
    fn test_double_macro_direct_ast() {
        // Manually create a double macro
        let macro_def = TopLevel {
            node: TopLevelKind::MacroDef {
                name: "double".to_string(),
                params: vec!["x".to_string()],
                body: Located::new(
                    ExprKind::Call {
                        name: "+".to_string(),
                        args: vec![
                            Located::new(ExprKind::Symbol("x".to_string()), Span::new(0, 0)),
                            Located::new(ExprKind::Symbol("x".to_string()), Span::new(0, 0)),
                        ],
                    },
                    Span::new(0, 0),
                ),
            },
            span: Span::new(0, 0),
        };

        // Create a variable definition using the macro
        let var_def = TopLevel {
            node: TopLevelKind::VarDef {
                name: "double-result".to_string(),
                value: Located::new(
                    ExprKind::Call {
                        name: "double".to_string(),
                        args: vec![
                            Located::new(
                                ExprKind::Literal(Literal::Integer(7)),
                                Span::new(0, 0),
                            ),
                        ],
                    },
                    Span::new(0, 0),
                ),
            },
            span: Span::new(0, 0),
        };

        // Create a program with the macro definition and variable definition
        let program = Program {
            forms: vec![macro_def, var_def],
        };

        // Process the program with the macro expander
        let mut expander = MacroExpander::new();
        let expanded_program = expander.process_program(&program);

        // Check that the double-result is now (+ 7 7) (expanded)
        assert_eq!(expanded_program.forms.len(), 1);
        match &expanded_program.forms[0].node {
            TopLevelKind::VarDef { name, value } => {
                assert_eq!(name, "double-result");
                match &value.node {
                    ExprKind::Call { name, args } => {
                        assert_eq!(name, "+");
                        assert_eq!(args.len(), 2);
                        for arg in args {
                            match &arg.node {
                                ExprKind::Literal(Literal::Integer(val)) => {
                                    assert_eq!(*val, 7);
                                },
                                _ => panic!("Expected Integer literal, got {:?}", arg.node),
                            }
                        }
                    },
                    _ => panic!("Expected Call expression, got {:?}", value.node),
                }
            },
            _ => panic!("Expected VarDef, got {:?}", expanded_program.forms[0].node),
        }
    }

    // Test constructing and expanding a rest parameter macro for list directly through AST construction
    #[test]
    fn test_list_macro_direct_ast() {
        // Manually create a list macro
        let macro_def = TopLevel {
            node: TopLevelKind::MacroDef {
                name: "list".to_string(),
                params: vec!["&".to_string(), "items".to_string()],
                body: Located::new(
                    ExprKind::Symbol("items".to_string()),
                    Span::new(0, 0),
                ),
            },
            span: Span::new(0, 0),
        };

        // Create a variable definition using the macro
        let var_def = TopLevel {
            node: TopLevelKind::VarDef {
                name: "list-result".to_string(),
                value: Located::new(
                    ExprKind::Call {
                        name: "list".to_string(),
                        args: vec![
                            Located::new(ExprKind::Literal(Literal::Integer(1)), Span::new(0, 0)),
                            Located::new(ExprKind::Literal(Literal::Integer(2)), Span::new(0, 0)),
                            Located::new(ExprKind::Literal(Literal::Integer(3)), Span::new(0, 0)),
                        ],
                    },
                    Span::new(0, 0),
                ),
            },
            span: Span::new(0, 0),
        };

        // Create a program with the macro definition and variable definition
        let program = Program {
            forms: vec![macro_def, var_def],
        };

        // Process the program with the macro expander
        let mut expander = MacroExpander::new();
        let expanded_program = expander.process_program(&program);

        // Check that the list-result is now a tuple [1, 2, 3] (expanded)
        assert_eq!(expanded_program.forms.len(), 1);
        match &expanded_program.forms[0].node {
            TopLevelKind::VarDef { name, value } => {
                assert_eq!(name, "list-result");
                match &value.node {
                    ExprKind::Literal(Literal::Tuple(items)) => {
                        assert_eq!(items.len(), 3);
                        for (i, item) in items.iter().enumerate() {
                            match &item.node {
                                ExprKind::Literal(Literal::Integer(val)) => {
                                    assert_eq!(*val, (i + 1) as i128);
                                },
                                _ => panic!("Expected Integer literal, got {:?}", item.node),
                            }
                        }
                    },
                    _ => panic!("Expected Tuple literal, got {:?}", value.node),
                }
            },
            _ => panic!("Expected VarDef, got {:?}", expanded_program.forms[0].node),
        }
    }

    // Test direct interpretation of expanded macros
    #[test]
    fn test_interpret_expanded_macros() {
        // Create a program with macros and usage
        let macro_defs = vec![
            // Identity macro
            TopLevel {
                node: TopLevelKind::MacroDef {
                    name: "identity".to_string(),
                    params: vec!["x".to_string()],
                    body: Located::new(
                        ExprKind::Symbol("x".to_string()),
                        Span::new(0, 0),
                    ),
                },
                span: Span::new(0, 0),
            },
            // Double macro
            TopLevel {
                node: TopLevelKind::MacroDef {
                    name: "double".to_string(),
                    params: vec!["x".to_string()],
                    body: Located::new(
                        ExprKind::Call {
                            name: "+".to_string(),
                            args: vec![
                                Located::new(ExprKind::Symbol("x".to_string()), Span::new(0, 0)),
                                Located::new(ExprKind::Symbol("x".to_string()), Span::new(0, 0)),
                            ],
                        },
                        Span::new(0, 0),
                    ),
                },
                span: Span::new(0, 0),
            },
            // Multiply macro
            TopLevel {
                node: TopLevelKind::MacroDef {
                    name: "mult".to_string(),
                    params: vec!["x".to_string(), "y".to_string()],
                    body: Located::new(
                        ExprKind::Call {
                            name: "*".to_string(),
                            args: vec![
                                Located::new(ExprKind::Symbol("x".to_string()), Span::new(0, 0)),
                                Located::new(ExprKind::Symbol("y".to_string()), Span::new(0, 0)),
                            ],
                        },
                        Span::new(0, 0),
                    ),
                },
                span: Span::new(0, 0),
            },
            // List macro
            TopLevel {
                node: TopLevelKind::MacroDef {
                    name: "list".to_string(),
                    params: vec!["&".to_string(), "items".to_string()],
                    body: Located::new(
                        ExprKind::Symbol("items".to_string()),
                        Span::new(0, 0),
                    ),
                },
                span: Span::new(0, 0),
            },
        ];

        // Create variable definitions using the macros
        let var_defs = vec![
            TopLevel {
                node: TopLevelKind::VarDef {
                    name: "id-result".to_string(),
                    value: Located::new(
                        ExprKind::Call {
                            name: "identity".to_string(),
                            args: vec![
                                Located::new(
                                    ExprKind::Literal(Literal::Integer(42)),
                                    Span::new(0, 0),
                                ),
                            ],
                        },
                        Span::new(0, 0),
                    ),
                },
                span: Span::new(0, 0),
            },
            TopLevel {
                node: TopLevelKind::VarDef {
                    name: "double-result".to_string(),
                    value: Located::new(
                        ExprKind::Call {
                            name: "double".to_string(),
                            args: vec![
                                Located::new(
                                    ExprKind::Literal(Literal::Integer(7)),
                                    Span::new(0, 0),
                                ),
                            ],
                        },
                        Span::new(0, 0),
                    ),
                },
                span: Span::new(0, 0),
            },
            TopLevel {
                node: TopLevelKind::VarDef {
                    name: "mult-result".to_string(),
                    value: Located::new(
                        ExprKind::Call {
                            name: "mult".to_string(),
                            args: vec![
                                Located::new(
                                    ExprKind::Literal(Literal::Integer(6)),
                                    Span::new(0, 0),
                                ),
                                Located::new(
                                    ExprKind::Literal(Literal::Integer(7)),
                                    Span::new(0, 0),
                                ),
                            ],
                        },
                        Span::new(0, 0),
                    ),
                },
                span: Span::new(0, 0),
            },
            TopLevel {
                node: TopLevelKind::VarDef {
                    name: "list-result".to_string(),
                    value: Located::new(
                        ExprKind::Call {
                            name: "list".to_string(),
                            args: vec![
                                Located::new(ExprKind::Literal(Literal::Integer(1)), Span::new(0, 0)),
                                Located::new(ExprKind::Literal(Literal::Integer(2)), Span::new(0, 0)),
                                Located::new(ExprKind::Literal(Literal::Integer(3)), Span::new(0, 0)),
                            ],
                        },
                        Span::new(0, 0),
                    ),
                },
                span: Span::new(0, 0),
            },
        ];

        // Combine the macro definitions and variable definitions
        let mut forms = Vec::new();
        forms.extend(macro_defs);
        forms.extend(var_defs);

        // Create a program with the macro definitions and variable definitions
        let program = Program { forms };

        // Process the program with the macro expander
        let mut expander = MacroExpander::new();
        let expanded_program = expander.process_program(&program);

        // Create an interpreter and register native functions
        let mut interpreter = Interpreter::new();
        interpreter.add_native_function("+", 2);
        interpreter.add_native_function("*", 2);

        // Execute the expanded program
        for form in &expanded_program.forms {
            interpreter.eval_top_level(form)
                .expect("Failed to evaluate form");
        }

        // Verify results
        // Check identity macro result
        let id_result = interpreter.get_var("id-result")
            .expect("id-result not found");
        match id_result {
            Value::Integer(val) => assert_eq!(val, 42),
            _ => panic!("id-result should be an integer, got: {:?}", id_result),
        }

        // Check double macro result
        let double_result = interpreter.get_var("double-result")
            .expect("double-result not found");
        match double_result {
            Value::Integer(val) => assert_eq!(val, 14),
            _ => panic!("double-result should be an integer, got: {:?}", double_result),
        }

        // Check multiply macro result
        let mult_result = interpreter.get_var("mult-result")
            .expect("mult-result not found");
        match mult_result {
            Value::Integer(val) => assert_eq!(val, 42),
            _ => panic!("mult-result should be an integer, got: {:?}", mult_result),
        }

        // Check list macro result
        let list_result = interpreter.get_var("list-result")
            .expect("list-result not found");
        match list_result {
            Value::Tuple(items) => {
                assert_eq!(items.len(), 3);
                for (i, item) in items.iter().enumerate() {
                    match item {
                        Value::Integer(val) => assert_eq!(*val, (i + 1) as i128),
                        _ => panic!("Expected integer in tuple, got: {:?}", item),
                    }
                }
            },
            _ => panic!("list-result should be a tuple, got: {:?}", list_result),
        }
    }
} 