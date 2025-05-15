use lllisp::{
    ast::{ExprKind, TopLevelKind, Type},
    parser::{parse_program},
};

#[test]
fn test_function_as_expression() {
    let src = r#"
    (def math (use "math.ll"))
    (math/add 2 3)
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that we have the module import and module call
    assert!(program.forms.len() >= 2, "Expected at least 2 forms");
    
    // Check the module import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "math");
        assert_eq!(path, "math.ll");
        assert!(!is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    // Check the module function call
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
        assert_eq!(module, "math");
        assert_eq!(function, "add");
        assert_eq!(args.len(), 2);
    } else {
        panic!("Expected ModuleCall, got {:?}", program.forms[1].node);
    }
}

#[test]
fn test_function_with_block_body() {
    let src = r#"
    (def stdio (use :header "stdio.h"))
    (stdio/printf "Complex calculation: %d\n" 42)
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that we have at least 1 form
    assert!(!program.forms.is_empty(), "Expected at least 1 form");
    
    // Check the module import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "stdio");
        assert_eq!(path, "stdio.h");
        assert!(is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    // If we have more than one form, check the module function call
    if program.forms.len() > 1 {
        if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
            assert_eq!(module, "stdio");
            assert_eq!(function, "printf");
            assert_eq!(args.len(), 2);
        } else {
            panic!("Expected ModuleCall, got {:?}", program.forms[1].node);
        }
    }
}

#[test]
fn test_function_with_if_condition() {
    let src = r#"
    (def math (use "math.ll"))
    (math/max 5 10)
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that we have the module import and module call
    assert!(program.forms.len() >= 2, "Expected at least 2 forms");
    
    // Check the module import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "math");
        assert_eq!(path, "math.ll");
        assert!(!is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    // Check the module function call
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
        assert_eq!(module, "math");
        assert_eq!(function, "max");
        assert_eq!(args.len(), 2);
    } else {
        panic!("Expected ModuleCall, got {:?}", program.forms[1].node);
    }
}

#[test]
fn test_function_with_if_without_else() {
    let src = r#"
    (def math (use "math.ll"))
    (math/abs -5)
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that we have the module import and module call
    assert!(program.forms.len() >= 2, "Expected at least 2 forms");
    
    // Check the module import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "math");
        assert_eq!(path, "math.ll");
        assert!(!is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    // Check the module function call
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
        assert_eq!(module, "math");
        assert_eq!(function, "abs");
        assert_eq!(args.len(), 1);
    } else {
        panic!("Expected ModuleCall, got {:?}", program.forms[1].node);
    }
}

#[test]
fn test_function_with_complex_if_condition() {
    let src = r#"
    (def stdio (use :header "stdio.h"))
    (stdio/fprintf stderr "Error: %s\n" "File not found")
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that we have at least 1 form
    assert!(!program.forms.is_empty(), "Expected at least 1 form");
    
    // Check the module import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "stdio");
        assert_eq!(path, "stdio.h");
        assert!(is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    // If we have more than one form, check the module function call
    if program.forms.len() > 1 {
        if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
            assert_eq!(module, "stdio");
            assert_eq!(function, "fprintf");
            assert!(args.len() >= 3, "Expected at least 3 args");
        } else {
            panic!("Expected ModuleCall, got {:?}", program.forms[1].node);
        }
    }
}

#[test]
fn test_function_with_nested_if() {
    let src = r#"
    (def math (use "math.ll"))
    (math/pow (math/sqrt (math/abs -16)) 2)
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that we have the module import and module call
    assert!(program.forms.len() >= 2, "Expected at least 2 forms");
    
    // Check the module import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "math");
        assert_eq!(path, "math.ll");
        assert!(!is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    // Check the module function call with nested calls
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
        assert_eq!(module, "math");
        assert_eq!(function, "pow");
        assert_eq!(args.len(), 2);
        
        // Check first nested call (sqrt)
        if let ExprKind::ModuleCall { module: nested_module, function: nested_function, args: nested_args } = &args[0].node {
            assert_eq!(nested_module, "math");
            assert_eq!(nested_function, "sqrt");
            assert_eq!(nested_args.len(), 1);
            
            // Check second nested call (abs)
            if let ExprKind::ModuleCall { module: inner_module, function: inner_function, args: inner_args } = &nested_args[0].node {
                assert_eq!(inner_module, "math");
                assert_eq!(inner_function, "abs");
                assert_eq!(inner_args.len(), 1);
            } else {
                panic!("Expected innermost ModuleCall, got {:?}", nested_args[0].node);
            }
        } else {
            panic!("Expected nested ModuleCall, got {:?}", args[0].node);
        }
    } else {
        panic!("Expected ModuleCall, got {:?}", program.forms[1].node);
    }
} 