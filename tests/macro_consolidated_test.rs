#[cfg(test)]
mod macro_consolidated_tests {
    use lllisp::ast::{ExprKind, Literal, Located, Span, Program, TopLevel, TopLevelKind};
    use lllisp::parser::parse_program;
    use lllisp::macro_expander::MacroExpander;
    use lllisp::interpreter::{Interpreter, Value};

    // Test macro parsing
    #[test]
    fn test_parse_macro_definitions() {
        // Test identity macro parsing
        let src = "(def identity (macro [x] x))";
        let result = parse_program(src).expect("Failed to parse identity macro");
        
        assert_eq!(result.forms.len(), 1);
        if let TopLevelKind::MacroDef { name, params, body } = &result.forms[0].node {
            assert_eq!(name, "identity");
            assert_eq!(params.len(), 1);
            assert_eq!(params[0], "x");
            
            match &body.node {
                ExprKind::Symbol(s) => assert_eq!(s, "x"),
                _ => panic!("Expected Symbol expression, got {:?}", body.node),
            }
        } else {
            panic!("Expected MacroDef, got {:?}", result.forms[0].node);
        }
        
        // Test double macro parsing
        let src = "(def double (macro [x] (+ x x)))";
        let result = parse_program(src).expect("Failed to parse double macro");
        
        assert_eq!(result.forms.len(), 1);
        if let TopLevelKind::MacroDef { name, params, body } = &result.forms[0].node {
            assert_eq!(name, "double");
            assert_eq!(params.len(), 1);
            assert_eq!(params[0], "x");
            
            match &body.node {
                ExprKind::Call { name, args } => {
                    assert_eq!(name, "+");
                    assert_eq!(args.len(), 2);
                    
                    for arg in args {
                        match &arg.node {
                            ExprKind::Symbol(s) => assert_eq!(s, "x"),
                            _ => panic!("Expected Symbol expression, got {:?}", arg.node),
                        }
                    }
                },
                _ => panic!("Expected Call expression, got {:?}", body.node),
            }
        } else {
            panic!("Expected MacroDef, got {:?}", result.forms[0].node);
        }
        
        // Test list macro parsing (with rest parameters)
        let src = "(def list (macro [& items] items))";
        let result = parse_program(src).expect("Failed to parse list macro");
        
        assert_eq!(result.forms.len(), 1);
        if let TopLevelKind::MacroDef { name, params, body } = &result.forms[0].node {
            assert_eq!(name, "list");
            assert_eq!(params.len(), 2);
            assert_eq!(params[0], "&");
            assert_eq!(params[1], "items");
            
            match &body.node {
                ExprKind::Symbol(s) => assert_eq!(s, "items"),
                _ => panic!("Expected Symbol expression, got {:?}", body.node),
            }
        } else {
            panic!("Expected MacroDef, got {:?}", result.forms[0].node);
        }
    }
    
    // Test direct macro expansion
    #[test]
    fn test_direct_macro_expansion() {
        // Create interpreter
        let mut interpreter = Interpreter::new();
        
        // Define the identity macro
        let identity_body = Located::new(
            ExprKind::Symbol("x".to_string()),
            Span::new(0, 0)
        );
        
        interpreter.define_macro("identity", vec!["x".to_string()], identity_body);
        
        // Create argument for the macro - literal 42
        let arg = Located::new(
            ExprKind::Literal(Literal::Integer(42)),
            Span::new(0, 0)
        );
        
        // Apply the macro
        let result = interpreter.macro_substitution("identity", &[arg])
            .expect("Failed to apply identity macro");
        
        // Check if the result is 42
        match &result.node {
            ExprKind::Literal(Literal::Integer(val)) => {
                assert_eq!(*val, 42);
            },
            _ => panic!("Expected integer literal 42, got {:?}", result.node),
        }
        
        // Define the double macro
        let double_body = Located::new(
            ExprKind::Call {
                name: "+".to_string(),
                args: vec![
                    Located::new(ExprKind::Symbol("x".to_string()), Span::new(0, 0)),
                    Located::new(ExprKind::Symbol("x".to_string()), Span::new(0, 0)),
                ],
            },
            Span::new(0, 0)
        );
        
        interpreter.define_macro("double", vec!["x".to_string()], double_body);
        
        // Create argument for the macro - literal 5
        let arg = Located::new(
            ExprKind::Literal(Literal::Integer(5)),
            Span::new(0, 0)
        );
        
        // Apply the macro
        let result = interpreter.macro_substitution("double", &[arg])
            .expect("Failed to substitute double macro");
        
        // Check if the result is (+ 5 5)
        match &result.node {
            ExprKind::Call { name, args } => {
                assert_eq!(name, "+");
                assert_eq!(args.len(), 2);
                
                // Both arguments should be 5
                for arg in args {
                    match &arg.node {
                        ExprKind::Literal(Literal::Integer(val)) => {
                            assert_eq!(*val, 5);
                        },
                        _ => panic!("Expected integer literal 5, got {:?}", arg.node),
                    }
                }
            },
            _ => panic!("Expected call expression, got {:?}", result.node),
        }
        
        // Define list macro with rest parameters
        let list_body = Located::new(
            ExprKind::Symbol("items".to_string()),
            Span::new(0, 0)
        );
        
        interpreter.define_macro("list", vec!["&".to_string(), "items".to_string()], list_body);
        
        // Create arguments for the macro
        let args = vec![
            Located::new(ExprKind::Literal(Literal::Integer(1)), Span::new(0, 0)),
            Located::new(ExprKind::Literal(Literal::Integer(2)), Span::new(0, 0)),
            Located::new(ExprKind::Literal(Literal::Integer(3)), Span::new(0, 0)),
        ];
        
        // Apply the macro
        let result = interpreter.macro_substitution("list", &args)
            .expect("Failed to apply list macro");
        
        // Expected result should be a tuple with [1, 2, 3]
        match &result.node {
            ExprKind::Literal(Literal::Tuple(items)) => {
                assert_eq!(items.len(), 3, "Expected 3 items in the tuple, got {}", items.len());
                
                // Check each item is the expected integer
                for (i, item) in items.iter().enumerate() {
                    match &item.node {
                        ExprKind::Literal(Literal::Integer(val)) => {
                            assert_eq!(*val, (i + 1) as i128);
                        },
                        _ => panic!("Expected integer literal, got {:?}", item.node),
                    }
                }
            },
            _ => {
                panic!("Expected tuple literal, got {:?}", result.node);
            }
        }
    }
    
    // Test macro expansion using MacroExpander
    #[test]
    fn test_macro_expansion_with_expander() {
        // Create a program with macros and their usage
        let src = std::fs::read_to_string("examples/basic_macros.lllisp")
            .expect("Failed to read basic_macros.lllisp file");
        
        // Parse the program
        let parsed_program = parse_program(&src)
            .expect("Failed to parse basic_macros.lllisp");
        
        // Process the program with the macro expander
        let mut expander = MacroExpander::new();
        let expanded_program = expander.process_program(&parsed_program);
        
        // Verify expansions - check that macro definitions are removed
        // and macro invocations are expanded
        let mut found_id_result = false;
        let mut found_double_result = false;
        let mut found_mult_result = false;
        let mut found_list_result = false;
        
        for form in &expanded_program.forms {
            if let TopLevelKind::VarDef { name, value } = &form.node {
                match name.as_str() {
                    "id-result" => {
                        found_id_result = true;
                        // Should be expanded to 42
                        match &value.node {
                            ExprKind::Literal(Literal::Integer(val)) => {
                                assert_eq!(*val, 42);
                            },
                            _ => panic!("Expected integer literal, got {:?}", value.node),
                        }
                    },
                    "double-result" => {
                        found_double_result = true;
                        // Should be expanded to (+ 7 7)
                        match &value.node {
                            ExprKind::Call { name, args } => {
                                assert_eq!(name, "+");
                                assert_eq!(args.len(), 2);
                                for arg in args {
                                    match &arg.node {
                                        ExprKind::Literal(Literal::Integer(val)) => {
                                            assert_eq!(*val, 7);
                                        },
                                        _ => panic!("Expected integer literal, got {:?}", arg.node),
                                    }
                                }
                            },
                            _ => panic!("Expected call expression, got {:?}", value.node),
                        }
                    },
                    "mult-result" => {
                        found_mult_result = true;
                        // Should be expanded to (* 6 7)
                        match &value.node {
                            ExprKind::Call { name, args } => {
                                assert_eq!(name, "*");
                                assert_eq!(args.len(), 2);
                                let mut found_6 = false;
                                let mut found_7 = false;
                                for arg in args {
                                    match &arg.node {
                                        ExprKind::Literal(Literal::Integer(val)) => {
                                            if *val == 6 { found_6 = true; }
                                            if *val == 7 { found_7 = true; }
                                        },
                                        _ => panic!("Expected integer literal, got {:?}", arg.node),
                                    }
                                }
                                assert!(found_6 && found_7, "Expected args 6 and 7");
                            },
                            _ => panic!("Expected call expression, got {:?}", value.node),
                        }
                    },
                    "my-list" => {
                        found_list_result = true;
                        // Should be expanded to tuple [1, 2, 3, 4, 5]
                        match &value.node {
                            ExprKind::Literal(Literal::Tuple(items)) => {
                                assert_eq!(items.len(), 5);
                                for (i, item) in items.iter().enumerate() {
                                    match &item.node {
                                        ExprKind::Literal(Literal::Integer(val)) => {
                                            assert_eq!(*val, (i + 1) as i128);
                                        },
                                        _ => panic!("Expected integer literal, got {:?}", item.node),
                                    }
                                }
                            },
                            _ => panic!("Expected tuple literal, got {:?}", value.node),
                        }
                    },
                    _ => {}
                }
            }
        }
        
        // Verify all expected expansions were found
        assert!(found_id_result, "id-result expansion not found");
        assert!(found_double_result, "double-result expansion not found");
        assert!(found_mult_result, "mult-result expansion not found");
        assert!(found_list_result, "my-list expansion not found");
    }
    
    // Test macro interpretation
    #[test]
    fn test_macro_interpretation() {
        let src = std::fs::read_to_string("examples/basic_macros.lllisp")
            .expect("Failed to read basic_macros.lllisp file");
        
        // Parse the program
        let parsed_program = parse_program(&src)
            .expect("Failed to parse basic_macros.lllisp");
        
        // Process macros
        let mut expander = MacroExpander::new();
        let expanded_program = expander.process_program(&parsed_program);
        
        // Create an interpreter and run the program
        let mut interpreter = Interpreter::new();
        
        // Register standard functions
        interpreter.add_native_function("+", 2);
        interpreter.add_native_function("*", 2);
        
        // Execute the program
        for form in &expanded_program.forms {
            interpreter.eval_top_level(form)
                .expect("Failed to evaluate form");
        }
        
        // Verify results
        let id_result = interpreter.get_var("id-result")
            .expect("id-result not found in interpreter");
        
        match id_result {
            Value::Integer(val) => assert_eq!(val, 42),
            _ => panic!("id-result should be an integer, got: {:?}", id_result),
        }
        
        let double_result = interpreter.get_var("double-result")
            .expect("double-result not found in interpreter");
        
        match double_result {
            Value::Integer(val) => assert_eq!(val, 14),
            _ => panic!("double-result should be an integer, got: {:?}", double_result),
        }
        
        let mult_result = interpreter.get_var("mult-result")
            .expect("mult-result not found in interpreter");
        
        match mult_result {
            Value::Integer(val) => assert_eq!(val, 42),
            _ => panic!("mult-result should be an integer, got: {:?}", mult_result),
        }
        
        let my_list = interpreter.get_var("my-list")
            .expect("my-list not found in interpreter");
        
        match my_list {
            Value::Tuple(items) => {
                assert_eq!(items.len(), 5);
                for (i, item) in items.iter().enumerate() {
                    match item {
                        Value::Integer(val) => assert_eq!(*val, (i as i128) + 1),
                        _ => panic!("Expected integer in tuple, got: {:?}", item),
                    }
                }
            },
            _ => panic!("my-list should be a tuple, got: {:?}", my_list),
        }
    }
} 