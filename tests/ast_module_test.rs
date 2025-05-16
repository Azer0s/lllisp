use lllisp::ast::{TopLevelKind, ExprKind, Literal};
use lllisp::parser::parse_program;
use lllisp::type_inference::TypeInferer;

#[test]
fn test_module_ast() {
    // Define a simple program with module imports
    let src = r#"
    (def stdio (use :header "stdio.h"))
    (def math (use "math"))
    "#;
    
    // Parse the program
    let parsed_program = parse_program(src).expect("Failed to parse program");
    
    // Verify the AST structure
    assert_eq!(parsed_program.forms.len(), 2);
    
    // Check the stdio module import (this is actually parsed as a VarDef)
    if let TopLevelKind::VarDef { name, value } = &parsed_program.forms[0].node {
        assert_eq!(name, "stdio");
        if let ExprKind::Call { name: call_name, args } = &value.node {
            assert_eq!(call_name, "use");
            assert_eq!(args.len(), 2);
            if let ExprKind::Literal(Literal::Atom(atom)) = &args[0].node {
                assert_eq!(atom, "header");
            } else {
                panic!("Expected first argument to be :header atom");
            }
            if let ExprKind::Literal(Literal::String(path)) = &args[1].node {
                assert_eq!(path, "stdio.h");
            } else {
                panic!("Expected second argument to be a string path");
            }
        } else {
            panic!("Expected value to be a Call to 'use'");
        }
    } else {
        panic!("Expected first form to be a VarDef");
    }
    
    // Check the math module import
    if let TopLevelKind::VarDef { name, value } = &parsed_program.forms[1].node {
        assert_eq!(name, "math");
        if let ExprKind::Call { name: call_name, args } = &value.node {
            assert_eq!(call_name, "use");
            assert_eq!(args.len(), 1);
            if let ExprKind::Literal(Literal::String(path)) = &args[0].node {
                assert_eq!(path, "math");
            } else {
                panic!("Expected argument to be a string path");
            }
        } else {
            panic!("Expected value to be a Call to 'use'");
        }
    } else {
        panic!("Expected second form to be a VarDef");
    }
    
    // Test type inference - just verify it doesn't fail
    let mut inferer = TypeInferer::new();
    let result = inferer.process_program(&parsed_program).expect("Type inference failed");
    
    // Verify the processed forms match the original ones
    assert_eq!(result.forms.len(), 2);
} 