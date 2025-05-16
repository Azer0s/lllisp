#[cfg(test)]
mod macro_tests {
    use lllisp::parser::parse_program;
    use lllisp::macro_expander::MacroExpander;
    use lllisp::interpreter::Interpreter;
    use lllisp::ast::{ExprKind, TopLevelKind, Literal};

    #[test]
    fn test_macros_expansion() {
        let src = std::fs::read_to_string("examples/basic_macros.lllisp")
            .expect("Failed to read basic_macros.lllisp file");
        
        // Parse the program
        let parsed_program = parse_program(&src)
            .expect("Failed to parse basic_macros.lllisp");
        
        println!("Parsed program forms count: {}", parsed_program.forms.len());
        
        // Process macros
        let mut expander = MacroExpander::new();
        let expanded_program = expander.process_program(&parsed_program);
        
        println!("Expanded program forms count: {}", expanded_program.forms.len());
        
        // Verify expansions
        let mut found_identity = false;
        let mut found_double = false;
        let mut found_list = false;
        
        for form in &expanded_program.forms {
            match &form.node {
                TopLevelKind::VarDef { name, value, .. } => {
                    println!("Checking var def: {}", name);
                    
                    if name == "id-result" {
                        // Should be expanded to 42
                        match &value.node {
                            ExprKind::Literal(Literal::Integer(val)) => {
                                assert_eq!(*val, 42);
                                found_identity = true;
                                println!("Found id-result = 42");
                            },
                            _ => panic!("id-result should be 42, got: {:?}", value.node),
                        }
                    } else if name == "double-result" {
                        // Should be expanded to (+ 7 7)
                        match &value.node {
                            ExprKind::Call { name: op, args } => {
                                assert_eq!(op, "+");
                                assert_eq!(args.len(), 2);
                                found_double = true;
                                println!("Found double-result = (+ 7 7)");
                            },
                            _ => panic!("double-result should be (+ 7 7), got: {:?}", value.node),
                        }
                    } else if name == "my-list" {
                        // Should be expanded to a tuple of 5 items
                        match &value.node {
                            ExprKind::Literal(Literal::Tuple(items)) => {
                                assert_eq!(items.len(), 5);
                                found_list = true;
                                println!("Found my-list with 5 items");
                            },
                            _ => panic!("my-list should be a tuple, got: {:?}", value.node),
                        }
                    }
                },
                _ => {}
            }
        }
        
        assert!(found_identity, "id-result expansion not found");
        assert!(found_double, "double-result expansion not found");
        assert!(found_list, "my-list expansion not found");
    }
    
    #[test]
    fn test_macros_interpretation() {
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
        
        // Execute the program
        for form in &expanded_program.forms {
            match &form.node {
                TopLevelKind::VarDef { name, .. } => {
                    println!("Executing var def: {}", name);
                    interpreter.eval_top_level(form)
                        .expect(&format!("Failed to evaluate {}", name));
                },
                _ => {
                    interpreter.eval_top_level(form)
                        .expect("Failed to evaluate expression");
                }
            }
        }
        
        // Verify results
        let id_result = interpreter.get_var("id-result")
            .expect("id-result not found in interpreter");
        
        match id_result {
            lllisp::interpreter::Value::Integer(val) => {
                assert_eq!(val, 42);
                println!("id-result = {}", val);
            },
            _ => panic!("id-result should be an integer, got: {:?}", id_result),
        }
        
        // Check the double result value (should be 14)
        let double_result = interpreter.get_var("double-result")
            .expect("double-result not found in interpreter");
        
        match double_result {
            lllisp::interpreter::Value::Integer(val) => {
                // Should be 14 (7 + 7)
                assert_eq!(val, 14);
                println!("double-result = {}", val);
            },
            _ => panic!("double-result should be an integer, got: {:?}", double_result),
        }
    }
} 