#[cfg(test)]
mod simple_macro_tests {
    use lllisp::parser::parse_program;
    use lllisp::macro_expander::MacroExpander;
    use lllisp::ast::{ExprKind, TopLevelKind, Literal};

    #[test]
    fn test_simple_macros() {
        let src = std::fs::read_to_string("examples/simple_macros.lllisp")
            .expect("Failed to read simple_macros.lllisp file");
        
        // Parse the program
        let parsed_program = parse_program(&src)
            .expect("Failed to parse simple_macros.lllisp");
        
        // Process macros
        let mut expander = MacroExpander::new();
        let expanded_program = expander.process_program(&parsed_program);
        
        // Check that expanded_program has the expected forms
        // Verify that result2 = (double 5) expanded to (+ 5 5)
        for form in &expanded_program.forms {
            if let TopLevelKind::VarDef { name, value } = &form.node {
                if name == "result2" {
                    if let ExprKind::Call { name: op, args } = &value.node {
                        assert_eq!(op, "+");
                        assert_eq!(args.len(), 2);
                        
                        // Check both arguments are 5
                        for arg in args {
                            if let ExprKind::Literal(Literal::Integer(val)) = &arg.node {
                                assert_eq!(*val, 5);
                            } else {
                                panic!("Expected integer literal, got {:?}", arg.node);
                            }
                        }
                        
                        println!("double macro expansion test passed!");
                        return;
                    } else {
                        panic!("Expected Call expression, got {:?}", value.node);
                    }
                }
            }
        }
        
        panic!("Could not find expanded macro in the result");
    }
} 