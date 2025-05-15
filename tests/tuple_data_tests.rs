use lllisp::{
    ast::{ExprKind, TopLevelKind, Type, Literal},
    parser::{parse_program, parse_form, expr_parser},
};
use chumsky::Parser;

#[test]
fn test_tuple_type_definition() {
    // Test tuple type definition
    let src = "(type http-resp (tuple http-resp-code (ptr u8)))";
    
    let form = parse_form(src).unwrap();
    
    if let TopLevelKind::TypeDef { name, ty } = &form.node {
        assert_eq!(name, "http-resp");
        
        if let Type::Tuple(types) = &ty {
            assert_eq!(types.len(), 2);
            
            // First element is a named type
            assert_eq!(types[0], Type::Named("http-resp-code".to_string()));
            
            // Second element is a pointer to u8
            assert_eq!(types[1], Type::Pointer(Box::new(Type::UInt(8))));
        } else {
            panic!("Expected Tuple type, got: {:?}", ty);
        }
    } else {
        panic!("Expected TypeDef, got: {:?}", form.node);
    }
}

#[test]
fn test_tuple_literal() {
    // Test tuple literal
    let src = "[1 2 3 4 5]";
    
    let expr = expr_parser().parse(src).unwrap();
    
    if let ExprKind::Literal(Literal::Tuple(elements)) = &expr.node {
        assert_eq!(elements.len(), 5);
        
        // Check the elements are integers 1 through 5
        for (i, elem) in elements.iter().enumerate() {
            if let ExprKind::Literal(Literal::Integer(n)) = &elem.node {
                assert_eq!(*n, (i + 1) as i128);
            } else {
                panic!("Expected Integer literal, got: {:?}", elem.node);
            }
        }
    } else {
        panic!("Expected Tuple literal, got: {:?}", expr.node);
    }
}

#[test]
fn test_mixed_tuple_literal() {
    // Test tuple with mixed types
    let src = "[42 \"Hello world\"]";
    
    let expr = expr_parser().parse(src).unwrap();
    
    if let ExprKind::Literal(Literal::Tuple(elements)) = &expr.node {
        assert_eq!(elements.len(), 2);
        
        // First element should be an integer 42
        if let ExprKind::Literal(Literal::Integer(n)) = &elements[0].node {
            assert_eq!(*n, 42);
        } else {
            panic!("Expected Integer literal, got: {:?}", elements[0].node);
        }
        
        // Second element should be a string "Hello world"
        if let ExprKind::Literal(Literal::String(s)) = &elements[1].node {
            assert_eq!(s, "Hello world");
        } else {
            panic!("Expected String literal, got: {:?}", elements[1].node);
        }
    } else {
        panic!("Expected Tuple literal, got: {:?}", expr.node);
    }
}

#[test]
fn test_data_type_definition() {
    // Test data type definition
    let src = r#"
    (type tok (data 
      [:ident (ptr u8)]
      [:whitespace]
    ))
    "#;
    
    let form = parse_form(src).unwrap();
    
    if let TopLevelKind::TypeDef { name, ty } = &form.node {
        assert_eq!(name, "tok");
        
        if let Type::Data(variants) = &ty {
            assert_eq!(variants.len(), 2);
            
            // First variant has a value
            assert_eq!(variants[0].0, "ident");
            assert_eq!(variants[0].1, Some(Type::Pointer(Box::new(Type::UInt(8)))));
            
            // Second variant has no value
            assert_eq!(variants[1].0, "whitespace");
            assert_eq!(variants[1].1, None);
        } else {
            panic!("Expected Data type, got: {:?}", ty);
        }
    } else {
        panic!("Expected TypeDef, got: {:?}", form.node);
    }
}

#[test]
fn test_data_constructor_with_value() {
    // Test data constructor with a value
    let src = "[:ident \"hello\"]";
    
    let expr = expr_parser().parse(src).unwrap();
    
    if let ExprKind::DataConstructor { tag, value } = &expr.node {
        assert_eq!(tag, "ident");
        
        // Check that value is a string "hello"
        if let Some(value_expr) = value {
            if let ExprKind::Literal(Literal::String(s)) = &value_expr.node {
                assert_eq!(s, "hello");
            } else {
                panic!("Expected String literal, got: {:?}", value_expr.node);
            }
        } else {
            panic!("Expected value for :ident variant");
        }
    } else {
        panic!("Expected DataConstructor, got: {:?}", expr.node);
    }
}

#[test]
fn test_data_constructor_without_value() {
    // Test data constructor without a value
    let src = "[:whitespace]";
    
    let expr = expr_parser().parse(src).unwrap();
    
    if let ExprKind::DataConstructor { tag, value } = &expr.node {
        assert_eq!(tag, "whitespace");
        
        // Check that there is no value
        assert!(value.is_none(), "Expected no value for :whitespace variant");
    } else {
        panic!("Expected DataConstructor, got: {:?}", expr.node);
    }
}

#[test]
fn test_data_constructor_with_atom() {
    // Test data constructor with an atom value
    let src = "[:status :ok]";
    
    let expr = expr_parser().parse(src).unwrap();
    
    if let ExprKind::DataConstructor { tag, value } = &expr.node {
        assert_eq!(tag, "status");
        
        // Check that value is an atom :ok
        if let Some(value_expr) = value {
            if let ExprKind::Literal(Literal::Atom(name)) = &value_expr.node {
                assert_eq!(name, "ok");
            } else {
                panic!("Expected Atom literal, got: {:?}", value_expr.node);
            }
        } else {
            panic!("Expected value for :status variant");
        }
    } else {
        panic!("Expected DataConstructor, got: {:?}", expr.node);
    }
}

#[test]
fn test_data_usage_in_program() {
    // Test using data types in a program
    let src = r#"
    (type tok (data 
      [:ident (ptr u8)]
      [:whitespace]
    ))
    (def x (tok [:ident "hello"]))
    "#;
    
    let program = parse_program(src).unwrap();
    assert_eq!(program.forms.len(), 2);
    
    // Check the variable definition
    if let TopLevelKind::VarDef { name, value } = &program.forms[1].node {
        assert_eq!(name, "x");
        
        if let ExprKind::Call { name, args } = &value.node {
            assert_eq!(name, "tok");
            assert_eq!(args.len(), 1);
            
            // Check the argument is a data constructor
            if let ExprKind::DataConstructor { tag, value } = &args[0].node {
                assert_eq!(tag, "ident");
                
                // Check that value is a string "hello"
                if let Some(value_expr) = value {
                    if let ExprKind::Literal(Literal::String(s)) = &value_expr.node {
                        assert_eq!(s, "hello");
                    } else {
                        panic!("Expected String literal, got: {:?}", value_expr.node);
                    }
                } else {
                    panic!("Expected value for :ident variant");
                }
            } else {
                panic!("Expected DataConstructor, got: {:?}", args[0].node);
            }
        } else {
            panic!("Expected Call expression, got: {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got: {:?}", program.forms[1].node);
    }
} 