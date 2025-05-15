use lllisp::ast::{ExprKind, TopLevelKind};
use lllisp::parser::parse_program;

#[test]
fn test_module_imports_and_calls() {
    let src = r#"
    (def stdio (use :header "stdio.h"))
    (def math (use "math.ll"))
    
    (stdio/printf "Hello, world!")
    (math/sqrt 16)
    "#;
    
    let program = parse_program(src).unwrap();
    println!("Parsed forms: {}", program.forms.len());
    
    // We should have at least 4 forms
    assert!(program.forms.len() >= 4, "Expected at least 4 forms, got {}", program.forms.len());
    
    // Check stdio import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "stdio");
        assert_eq!(path, "stdio.h");
        assert!(is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    // Check math import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[1].node {
        assert_eq!(name, "math");
        assert_eq!(path, "math.ll");
        assert!(!is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[1].node);
    }
    
    // Check stdio/printf module call
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[2].node {
        assert_eq!(module, "stdio");
        assert_eq!(function, "printf");
        assert_eq!(args.len(), 1);
    } else {
        panic!("Expected ModuleCall, got {:?}", program.forms[2].node);
    }
    
    // Check math/sqrt module call
    if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[3].node {
        assert_eq!(module, "math");
        assert_eq!(function, "sqrt");
        assert_eq!(args.len(), 1);
    } else {
        panic!("Expected ModuleCall, got {:?}", program.forms[3].node);
    }
} 