use lllisp::ast::TopLevelKind;
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
    
    // Check the stdio module import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &parsed_program.forms[0].node {
        assert_eq!(name, "stdio");
        assert_eq!(path, "stdio.h");
        assert!(is_header);
    } else {
        panic!("Expected first form to be a ModuleImport");
    }
    
    // Check the math module import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &parsed_program.forms[1].node {
        assert_eq!(name, "math");
        assert_eq!(path, "math");
        assert!(!is_header);
    } else {
        panic!("Expected second form to be a ModuleImport");
    }
    
    // Test type inference - just verify it doesn't fail
    let mut inferer = TypeInferer::new();
    let result = inferer.process_program(&parsed_program).expect("Type inference failed");
    
    // Verify the processed forms match the original ones
    assert_eq!(result.forms.len(), 2);
} 