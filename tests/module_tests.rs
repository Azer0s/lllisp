use lllisp::ast::{TopLevelKind, ExprKind};
use lllisp::parser::parse_program;
use std::fs;

#[test]
fn test_module_imports() {
    let source = fs::read_to_string("tests/module_imports.lllisp").expect("Failed to read test file");
    let program = parse_program(&source).expect("Failed to parse program");
    
    // Print how many forms we got
    println!("Number of parsed forms: {}", program.forms.len());
    
    // Print each form for debugging
    for (i, form) in program.forms.iter().enumerate() {
        println!("Form {}: {:?}", i, form.node);
    }
    
    // For now, just check that we have at least the module imports
    assert!(program.forms.len() >= 4, "Should parse at least 4 forms (the imports)");
    
    // Test the first module import (stdio)
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "stdio");
        
        if let ExprKind::Call { name: call_name, args } = &value.node {
            assert_eq!(call_name, "use");
            assert_eq!(args.len(), 2);  // :header "stdio.h"
        } else {
            panic!("Expected Call to 'use', got {:?}", value.node);
        }
    } else {
        panic!("Expected first form to be a VarDef, got {:?}", program.forms[0].node);
    }
    
    // Test the second module import (other)
    if let TopLevelKind::VarDef { name, value } = &program.forms[1].node {
        assert_eq!(name, "other");
        
        if let ExprKind::Call { name: call_name, args } = &value.node {
            assert_eq!(call_name, "use");
            assert_eq!(args.len(), 1);  // "other"
        } else {
            panic!("Expected Call to 'use', got {:?}", value.node);
        }
    } else {
        panic!("Expected second form to be a VarDef, got {:?}", program.forms[1].node);
    }
    
    // Test the third module import (subdir-other)
    if let TopLevelKind::VarDef { name, value } = &program.forms[2].node {
        assert_eq!(name, "subdir-other");
        
        if let ExprKind::Call { name: call_name, args } = &value.node {
            assert_eq!(call_name, "use");
            assert_eq!(args.len(), 1);  // "subdir/other"
        } else {
            panic!("Expected Call to 'use', got {:?}", value.node);
        }
    } else {
        panic!("Expected third form to be a VarDef, got {:?}", program.forms[2].node);
    }
    
    // Test the fourth module import (subdir/foo)
    if let TopLevelKind::VarDef { name, value } = &program.forms[3].node {
        assert_eq!(name, "subdir/foo");
        
        if let ExprKind::Call { name: call_name, args } = &value.node {
            assert_eq!(call_name, "use");
            assert_eq!(args.len(), 1);  // "subdir/foo"
        } else {
            panic!("Expected Call to 'use', got {:?}", value.node);
        }
    } else {
        panic!("Expected fourth form to be a VarDef, got {:?}", program.forms[3].node);
    }
} 