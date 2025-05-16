#[cfg(test)]
mod macro_parser_tests {
    use lllisp::parser::parse_form;
    use lllisp::ast::{TopLevelKind, ExprKind};

    #[test]
    fn test_parse_identity_macro() {
        // Try to parse a simple identity macro
        let src = "(def identity (macro [x] x))";
        
        let result = parse_form(src).expect("Failed to parse identity macro");
        
        // Check it's a MacroDef with the right attributes
        match &result.node {
            TopLevelKind::MacroDef { name, params, body } => {
                assert_eq!(name, "identity");
                assert_eq!(params.len(), 1);
                assert_eq!(params[0], "x");
                
                // Body should be a Symbol expression with the value "x"
                match &body.node {
                    ExprKind::Symbol(s) => {
                        assert_eq!(s, "x");
                    },
                    _ => panic!("Expected Symbol expression, got {:?}", body.node),
                }
                
                println!("Successfully parsed identity macro!");
            },
            _ => panic!("Expected MacroDef, got {:?}", result.node),
        }
    }
    
    #[test]
    fn test_parse_double_macro() {
        // Try to parse a double macro
        let src = "(def double (macro [x] (+ x x)))";
        
        let result = parse_form(src).expect("Failed to parse double macro");
        
        // Check it's a MacroDef with the right attributes
        match &result.node {
            TopLevelKind::MacroDef { name, params, body } => {
                assert_eq!(name, "double");
                assert_eq!(params.len(), 1);
                assert_eq!(params[0], "x");
                
                // Body should be a Call expression (+ x x)
                match &body.node {
                    ExprKind::Call { name, args } => {
                        assert_eq!(name, "+");
                        assert_eq!(args.len(), 2);
                        
                        // Both arguments should be 'x'
                        for arg in args {
                            match &arg.node {
                                ExprKind::Symbol(s) => {
                                    assert_eq!(s, "x");
                                },
                                _ => panic!("Expected Symbol expression, got {:?}", arg.node),
                            }
                        }
                    },
                    _ => panic!("Expected Call expression, got {:?}", body.node),
                }
                
                println!("Successfully parsed double macro!");
            },
            _ => panic!("Expected MacroDef, got {:?}", result.node),
        }
    }
    
    // Create a simple test for the interpreter-based expansion
    #[test]
    fn test_expand_identity_macro() {
        use lllisp::parser::parse_program;
        use lllisp::macro_expander::MacroExpander;
        use lllisp::ast::Literal;
        
        // Define and use a simple identity macro
        let src = "(def identity (macro [x] x)) (def result (identity 42))";
        
        // Parse the program
        let parsed = parse_program(src).expect("Failed to parse program");
        
        // Process with the macro expander
        let mut expander = MacroExpander::new();
        let expanded = expander.process_program(&parsed);
        
        // We should have one top-level form - the result definition
        assert_eq!(expanded.forms.len(), 1);
        
        // Check the result is 42
        if let TopLevelKind::VarDef { name, value } = &expanded.forms[0].node {
            assert_eq!(name, "result");
            
            if let ExprKind::Literal(Literal::Integer(val)) = &value.node {
                assert_eq!(*val, 42);
                println!("Successfully expanded identity macro!");
            } else {
                panic!("Expected integer literal 42, got {:?}", value.node);
            }
        } else {
            panic!("Expected VarDef, got {:?}", expanded.forms[0].node);
        }
    }
} 