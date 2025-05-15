use lllisp::{
    ast::{ExprKind, TopLevelKind, Type},
    parser::{parse_program},
};

#[test]
fn test_basic_function() {
    let src = "(def math (use \"math.ll\")) (math/sqrt 16)";
    
    let program = parse_program(src).unwrap();
    println!("Program forms: {:?}", program.forms);
    
    assert!(program.forms.len() >= 2, "Expected at least 2 forms");
    
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "math");
        assert_eq!(path, "math.ll");
        assert!(!is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
        assert_eq!(module, "math");
        assert_eq!(function, "sqrt");
        assert_eq!(args.len(), 1);
    } else {
        panic!("Expected ModuleCall, got {:?}", program.forms[1].node);
    }
}

#[test]
fn test_two_params() {
    let src = "(def math (use \"math.ll\")) (math/pow 2 3)";
    
    let program = parse_program(src).unwrap();
    println!("Program forms: {:?}", program.forms);
    
    assert!(program.forms.len() >= 2, "Expected at least 2 forms");
    
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "math");
        assert_eq!(path, "math.ll");
        assert!(!is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
        assert_eq!(module, "math");
        assert_eq!(function, "pow");
        assert_eq!(args.len(), 2);
    } else {
        panic!("Expected ModuleCall, got {:?}", program.forms[1].node);
    }
}

#[test]
fn test_function_param() {
    let src = "(def stdio (use :header \"stdio.h\")) (stdio/printf \"test\")";
    
    let program = parse_program(src).unwrap();
    println!("Program forms for test_function_param: {:?}", program.forms);
    
    assert!(program.forms.len() >= 2, "Expected at least 2 forms");
    
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "stdio");
        assert_eq!(path, "stdio.h");
        assert!(is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
        assert_eq!(module, "stdio");
        assert_eq!(function, "printf");
        assert_eq!(args.len(), 1);
    } else {
        panic!("Expected ModuleCall, got {:?}", program.forms[1].node);
    }
}

#[test]
fn test_function_expression() {
    let src = "(def stdio (use :header \"stdio.h\")) (stdio/fprintf stderr \"Error\")";
    
    let program = parse_program(src).unwrap();
    println!("Program forms for test_function_expression: {:?}", program.forms);
    
    assert!(program.forms.len() >= 2, "Expected at least 2 forms");
    
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "stdio");
        assert_eq!(path, "stdio.h");
        assert!(is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
        assert_eq!(module, "stdio");
        assert_eq!(function, "fprintf");
        assert_eq!(args.len(), 2);
    } else {
        panic!("Expected ModuleCall, got {:?}", program.forms[1].node);
    }
}

#[test]
fn test_simple_function_type() {
    let src = "(def math (use \"math.ll\")) (math/max (math/sqrt 16) 5)";
    
    let program = parse_program(src).unwrap();
    println!("Program forms for test_simple_function_type: {:?}", program.forms);
    
    assert!(program.forms.len() >= 2, "Expected at least 2 forms");
    
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "math");
        assert_eq!(path, "math.ll");
        assert!(!is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[1].node {
        assert_eq!(module, "math");
        assert_eq!(function, "max");
        assert_eq!(args.len(), 2);
        
        if let ExprKind::ModuleCall { module: nested_module, function: nested_function, .. } = &args[0].node {
            assert_eq!(nested_module, "math");
            assert_eq!(nested_function, "sqrt");
        } else {
            panic!("Expected nested ModuleCall, got {:?}", args[0].node);
        }
    } else {
        panic!("Expected ModuleCall, got {:?}", program.forms[1].node);
    }
} 