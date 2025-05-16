#[cfg(test)]
mod comprehensive_macro_tests {
    
    use lllisp::macro_expander::MacroExpander;
    use lllisp::ast::{ExprKind, TopLevelKind, Literal, Program, Located, Span, TopLevel};
    use lllisp::interpreter::{Interpreter, Value};

    // Helper function to create simple expression AST nodes
    fn expr(kind: ExprKind) -> Located<ExprKind> {
        Located::new(kind, Span::new(0, 0))
    }

    // Helper function to create simple top-level AST nodes
    fn top_level(kind: TopLevelKind) -> TopLevel {
        Located::new(kind, Span::new(0, 0))
    }

    #[test]
    fn test_comprehensive_macros_expansion_direct() {
        // Create a program AST directly instead of parsing a file
        let mut program = Program { forms: Vec::new() };

        // Define identity macro: (def identity (macro [x] x))
        let identity_macro = top_level(TopLevelKind::MacroDef { 
            name: "identity".to_string(), 
            params: vec!["x".to_string()], 
            body: expr(ExprKind::Symbol("x".to_string())) 
        });

        // Define double macro: (def double (macro [x] (+ x x)))
        let double_macro = top_level(TopLevelKind::MacroDef { 
            name: "double".to_string(), 
            params: vec!["x".to_string()], 
            body: expr(ExprKind::Call { 
                name: "+".to_string(), 
                args: vec![
                    expr(ExprKind::Symbol("x".to_string())), 
                    expr(ExprKind::Symbol("x".to_string()))
                ] 
            }) 
        });

        // Define mult macro: (def mult (macro [x y] (* x y)))
        let mult_macro = top_level(TopLevelKind::MacroDef { 
            name: "mult".to_string(), 
            params: vec!["x".to_string(), "y".to_string()], 
            body: expr(ExprKind::Call { 
                name: "*".to_string(), 
                args: vec![
                    expr(ExprKind::Symbol("x".to_string())), 
                    expr(ExprKind::Symbol("y".to_string()))
                ] 
            }) 
        });

        // Define list macro: (def list (macro [& items] items))
        let list_macro = top_level(TopLevelKind::MacroDef { 
            name: "list".to_string(), 
            params: vec!["&".to_string(), "items".to_string()], 
            body: expr(ExprKind::Symbol("items".to_string())) 
        });

        // Add the macro definitions to the program
        program.forms.push(identity_macro);
        program.forms.push(double_macro);
        program.forms.push(mult_macro);
        program.forms.push(list_macro);

        // Add usage of macros
        // (def result1 (identity 42))
        let result1 = top_level(TopLevelKind::VarDef { 
            name: "result1".to_string(), 
            value: expr(ExprKind::Call { 
                name: "identity".to_string(), 
                args: vec![expr(ExprKind::Literal(Literal::Integer(42)))] 
            }) 
        });

        // (def result2 (double 7))
        let result2 = top_level(TopLevelKind::VarDef { 
            name: "result2".to_string(), 
            value: expr(ExprKind::Call { 
                name: "double".to_string(), 
                args: vec![expr(ExprKind::Literal(Literal::Integer(7)))] 
            }) 
        });

        // (def result3 (mult 6 7))
        let result3 = top_level(TopLevelKind::VarDef { 
            name: "result3".to_string(), 
            value: expr(ExprKind::Call { 
                name: "mult".to_string(), 
                args: vec![
                    expr(ExprKind::Literal(Literal::Integer(6))),
                    expr(ExprKind::Literal(Literal::Integer(7)))
                ] 
            }) 
        });

        // (def result4 (list 1 2 3 4 5))
        let result4 = top_level(TopLevelKind::VarDef { 
            name: "result4".to_string(), 
            value: expr(ExprKind::Call { 
                name: "list".to_string(), 
                args: vec![
                    expr(ExprKind::Literal(Literal::Integer(1))),
                    expr(ExprKind::Literal(Literal::Integer(2))),
                    expr(ExprKind::Literal(Literal::Integer(3))),
                    expr(ExprKind::Literal(Literal::Integer(4))),
                    expr(ExprKind::Literal(Literal::Integer(5)))
                ] 
            }) 
        });

        // Add the usage to the program
        program.forms.push(result1);
        program.forms.push(result2);
        program.forms.push(result3);
        program.forms.push(result4);

        // Process macros
        let mut expander = MacroExpander::new();
        let expanded_program = expander.process_program(&program);
        
        println!("Expanded program forms count: {}", expanded_program.forms.len());
        
        // Verify expansion of basic macros
        let mut found_identity = false;
        let mut found_double = false;
        let mut found_mult = false;
        let mut found_list = false;
        
        for form in &expanded_program.forms {
            match &form.node {
                TopLevelKind::VarDef { name, value, .. } => {
                    println!("Checking var def: {}", name);
                    
                    if name == "result1" {
                        // Should be expanded to 42
                        match &value.node {
                            ExprKind::Literal(Literal::Integer(val)) => {
                                assert_eq!(*val, 42);
                                found_identity = true;
                                println!("✓ Found result1 = 42");
                            },
                            _ => panic!("result1 should be 42, got: {:?}", value.node),
                        }
                    } else if name == "result2" {
                        // Should be expanded to (+ 7 7)
                        match &value.node {
                            ExprKind::Call { name: op, args } => {
                                assert_eq!(op, "+");
                                assert_eq!(args.len(), 2);
                                found_double = true;
                                println!("✓ Found result2 = (+ 7 7)");
                            },
                            _ => panic!("result2 should be (+ 7 7), got: {:?}", value.node),
                        }
                    } else if name == "result3" {
                        // Should be expanded to (* 6 7)
                        match &value.node {
                            ExprKind::Call { name: op, args } => {
                                assert_eq!(op, "*");
                                assert_eq!(args.len(), 2);
                                found_mult = true;
                                println!("✓ Found result3 = (* 6 7)");
                            },
                            _ => panic!("result3 should be (* 6 7), got: {:?}", value.node),
                        }
                    } else if name == "result4" {
                        // Should be expanded to a tuple of 5 items
                        match &value.node {
                            ExprKind::Literal(Literal::Tuple(items)) => {
                                assert_eq!(items.len(), 5);
                                found_list = true;
                                println!("✓ Found result4 with 5 items");
                            },
                            _ => panic!("result4 should be a tuple, got: {:?}", value.node),
                        }
                    }
                },
                _ => {}
            }
        }
        
        assert!(found_identity, "identity macro expansion not found");
        assert!(found_double, "double macro expansion not found");
        assert!(found_mult, "mult macro expansion not found");
        assert!(found_list, "list macro expansion not found");
    }
    
    #[test]
    fn test_file_based_macro_expansion() {
        // This test verifies that our example file can be parsed successfully
        let result = std::fs::read_to_string("examples/comprehensive_macros.lllisp");
        assert!(result.is_ok(), "Failed to read the comprehensive_macros.lllisp file");
        
        let src = result.unwrap();
        let parse_result = lllisp::parser::parse_program(&src);
        assert!(parse_result.is_ok(), "Failed to parse comprehensive_macros.lllisp: {:?}", parse_result.err());
        
        let program = parse_result.unwrap();
        assert!(!program.forms.is_empty(), "No forms found in the parsed program");
        
        // Verify that we have some macro definitions
        let mut found_macro_def = false;
        
        for form in &program.forms {
            if let TopLevelKind::MacroDef { .. } = &form.node {
                found_macro_def = true;
                break;
            }
        }
        
        assert!(found_macro_def, "No macro definitions found in the parsed program");
    }
} 