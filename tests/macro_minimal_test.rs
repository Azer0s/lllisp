#[cfg(test)]
mod macro_minimal_tests {
    use lllisp::ast::{ExprKind, Literal, Located, Span, TopLevel, TopLevelKind, Program};
    use lllisp::macro_expander::MacroExpander;
    

    #[test]
    fn test_identity_macro() {
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
        for form in &expanded_program.forms {
            if let TopLevelKind::VarDef { name, value, .. } = &form.node {
                if name == "id-result" {
                    match &value.node {
                        ExprKind::Literal(Literal::Integer(val)) => {
                            assert_eq!(*val, 42);
                            println!("Identity macro expanded to 42!");
                        },
                        _ => panic!("Expected integer literal 42, got: {:?}", value.node),
                    }
                }
            }
        }
    }

    #[test]
    fn test_double_macro() {
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
        for form in &expanded_program.forms {
            if let TopLevelKind::VarDef { name, value, .. } = &form.node {
                if name == "double-result" {
                    match &value.node {
                        ExprKind::Call { name: op, args } => {
                            assert_eq!(op, "+");
                            assert_eq!(args.len(), 2);
                            
                            // Both arguments should be 7
                            for arg in args {
                                match &arg.node {
                                    ExprKind::Literal(Literal::Integer(val)) => {
                                        assert_eq!(*val, 7);
                                    },
                                    _ => panic!("Expected integer literal 7, got: {:?}", arg.node),
                                }
                            }
                            
                            println!("Double macro expanded to (+ 7 7)!");
                        },
                        _ => panic!("Expected Call expression, got: {:?}", value.node),
                    }
                }
            }
        }
    }
    
    #[test]
    fn test_rest_params_macro() {
        // Manually create a list macro with rest parameters
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
                name: "my-list".to_string(),
                value: Located::new(
                    ExprKind::Call {
                        name: "list".to_string(),
                        args: vec![
                            Located::new(
                                ExprKind::Literal(Literal::Integer(1)),
                                Span::new(0, 0),
                            ),
                            Located::new(
                                ExprKind::Literal(Literal::Integer(2)),
                                Span::new(0, 0),
                            ),
                            Located::new(
                                ExprKind::Literal(Literal::Integer(3)),
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

        // Check that the my-list is now a tuple with [1, 2, 3] (expanded)
        for form in &expanded_program.forms {
            if let TopLevelKind::VarDef { name, value, .. } = &form.node {
                if name == "my-list" {
                    match &value.node {
                        ExprKind::Literal(Literal::Tuple(items)) => {
                            assert_eq!(items.len(), 3);
                            
                            // Check each item is the expected integer
                            for (i, item) in items.iter().enumerate() {
                                match &item.node {
                                    ExprKind::Literal(Literal::Integer(val)) => {
                                        assert_eq!(*val, (i + 1) as i128);
                                    },
                                    _ => panic!("Expected integer literal, got: {:?}", item.node),
                                }
                            }
                            
                            println!("List macro expanded to tuple with 3 items!");
                        },
                        _ => panic!("Expected Tuple literal, got: {:?}", value.node),
                    }
                }
            }
        }
    }
} 