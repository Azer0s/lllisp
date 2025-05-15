use lllisp::ast::{ExprKind, TopLevelKind};
use lllisp::parser::{parse_program, parse_module_call};
use std::fs;

#[test]
fn test_debug_module_call() {
    // Read the test file
    let source = fs::read_to_string("tests/module_call_debug.lllisp").expect("Failed to read test file");
    println!("Source code to parse: {}", source);
    
    // Parse the program
    let program = parse_program(&source).expect("Failed to parse program");
    
    // Print debug info
    println!("Parsed {} forms", program.forms.len());
    for (i, form) in program.forms.iter().enumerate() {
        println!("Form {}: {:?}", i, form.node);
    }
    
    // Check that we have at least two forms (the import and the function call)
    assert_eq!(program.forms.len(), 2, "Expected 2 forms, got {}", program.forms.len());
    
    // Check the module import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        println!("Found module import: {} -> {} (header: {})", name, path, is_header);
        assert_eq!(name, "math");
        assert_eq!(path, "math.ll");
        assert!(!is_header);
    } else {
        panic!("Expected first form to be a ModuleImport, got {:?}", program.forms[0].node);
    }
    
    // Check the module function call
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
        println!("Found module call: {}/{} with {} args", module, function, args.len());
        assert_eq!(module, "math");
        assert_eq!(function, "sqrt");
        assert_eq!(args.len(), 1);
    } else {
        println!("ERROR: Didn't find expected ModuleCall");
        if let TopLevelKind::Expr(expr) = &program.forms[1].node {
            println!("Found expression of type: {:?}", expr);
        }
        panic!("Expected second form to be a ModuleCall, got {:?}", program.forms[1].node);
    }
    
    // Direct test of the module call parser
    let module_call_src = "(math/sqrt 16)";
    println!("\nDirectly parsing module call: {}", module_call_src);
    
    let result = parse_module_call(module_call_src);
    match result {
        Ok(expr) => {
            println!("Successfully parsed as: {:?}", expr.node);
            if let ExprKind::ModuleCall { module, function, args } = &expr.node {
                assert_eq!(module, "math");
                assert_eq!(function, "sqrt");
                assert_eq!(args.len(), 1);
            } else {
                panic!("Expected ModuleCall, got {:?}", expr.node);
            }
        },
        Err(errors) => {
            for error in errors {
                println!("Parser error: {}", error);
            }
            panic!("Failed to parse module call directly");
        }
    }
} 