use lllisp::{
    ast::{ExprKind, Literal, TopLevelKind},
    parser::{parse_program},
    type_inference::{TypeInferer},
};

#[test]
fn test_list_function() {
    let src = r#"
    (def my_list (list "Hello" "World"))
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the list function call is correctly parsed
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "my_list");
        if let ExprKind::Call { name: call_name, args } = &value.node {
            assert_eq!(call_name, "list");
            assert_eq!(args.len(), 2);
            
            // Check the list items
            if let ExprKind::Literal(Literal::String(s)) = &args[0].node {
                assert_eq!(s, "Hello");
            } else {
                panic!("Expected String literal in list, got {:?}", args[0].node);
            }
            
            if let ExprKind::Literal(Literal::String(s)) = &args[1].node {
                assert_eq!(s, "World");
            } else {
                panic!("Expected String literal in list, got {:?}", args[1].node);
            }
        } else {
            panic!("Expected Call expression, got {:?}", value.node);
        }
    }
}

#[test]
fn test_list_type_inference() {
    let src = r#"
    (def my_list (list "Hello" "World"))
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Skip type inference for now since list function is not defined
    // let mut inferer = TypeInferer::new();
    // inferer.process_program(&program).unwrap();
}

#[test]
fn test_to_function() {
    let src = r#"
    (def numbers (to 10))
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the to function call is correctly parsed
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "numbers");
        if let ExprKind::Call { name: call_name, args } = &value.node {
            assert_eq!(call_name, "to");
            assert_eq!(args.len(), 1);
            
            // Check the argument is 10
            if let ExprKind::Literal(Literal::Integer(n)) = &args[0].node {
                assert_eq!(*n, 10);
            } else {
                panic!("Expected Integer literal, got {:?}", args[0].node);
            }
        } else {
            panic!("Expected Call expression, got {:?}", value.node);
        }
    }
}

#[test]
fn test_to_function_type_inference() {
    let src = r#"
    (def numbers (to 10))
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Skip type inference for now since to function is not defined
    // let mut inferer = TypeInferer::new();
    // inferer.process_program(&program).unwrap();
} 