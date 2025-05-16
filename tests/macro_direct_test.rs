#[cfg(test)]
mod macro_direct_tests {
    use lllisp::ast::{ExprKind, Literal, Located, Span};
    use lllisp::interpreter::Interpreter;

    // Directly test the macro expansion through the interpreter
    #[test]
    fn test_direct_identity_macro() {
        // Create interpreter
        let mut interpreter = Interpreter::new();
        
        // Define the identity macro
        let macro_body = Located::new(
            ExprKind::Symbol("x".to_string()),
            Span::new(0, 0)
        );
        
        interpreter.define_macro("identity", vec!["x".to_string()], macro_body);
        
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
                println!("Identity macro expansion successful!");
            },
            _ => panic!("Expected integer literal 42, got {:?}", result.node),
        }
    }
    
    // Test double macro using the macro_substitution method instead of evaluation
    #[test]
    fn test_direct_double_macro() {
        // Create interpreter
        let mut interpreter = Interpreter::new();
        
        // Define the double macro with body (+ x x)
        let macro_body = Located::new(
            ExprKind::Call {
                name: "+".to_string(),
                args: vec![
                    Located::new(ExprKind::Symbol("x".to_string()), Span::new(0, 0)),
                    Located::new(ExprKind::Symbol("x".to_string()), Span::new(0, 0)),
                ],
            },
            Span::new(0, 0)
        );
        
        interpreter.define_macro("double", vec!["x".to_string()], macro_body);
        
        // Create argument for the macro - literal 5
        let arg = Located::new(
            ExprKind::Literal(Literal::Integer(5)),
            Span::new(0, 0)
        );
        
        // Apply the macro through direct substitution
        let result = interpreter.macro_substitution("double", &[arg])
            .expect("Failed to substitute double macro");
        
        // Check if the result is (+ 5 5) - just check the structure
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
                
                println!("Double macro expansion successful!");
            },
            _ => panic!("Expected call expression, got {:?}", result.node),
        }
    }
    
    // Test a macro using rest parameters with direct items symbol approach
    #[test]
    fn test_rest_params_macro() {
        // Create interpreter
        let mut interpreter = Interpreter::new();
        
        // Create a body that references the "items" parameter directly
        let items_symbol_expr = Located::new(
            ExprKind::Symbol("items".to_string()),
            Span::new(0, 0)
        );
        
        // Register the macro with rest parameters
        interpreter.define_macro("list", vec!["&".to_string(), "items".to_string()], items_symbol_expr);
        
        // Create arguments for the macro
        let args = vec![
            Located::new(ExprKind::Literal(Literal::Integer(1)), Span::new(0, 0)),
            Located::new(ExprKind::Literal(Literal::Integer(2)), Span::new(0, 0)),
            Located::new(ExprKind::Literal(Literal::Integer(3)), Span::new(0, 0)),
        ];
        
        // Apply the macro
        let result = interpreter.macro_substitution("list", &args)
            .expect("Failed to apply list macro");
        
        // Print the result
        println!("Result of macro substitution: {:?}", result.node);
        
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
                
                println!("Rest parameters macro expansion successful!");
            },
            _ => {
                panic!("Expected tuple literal, got {:?}", result.node);
            }
        }
    }
} 