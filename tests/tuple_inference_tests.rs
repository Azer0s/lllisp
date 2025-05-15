use lllisp::{
    ast::{Type, TopLevelKind, ExprKind, Literal},
    parser::parse_program,
    type_inference::TypeInferer,
};

#[test]
fn test_tuple_type_inference() {
    let src = r#"
    (def mixed_tuple [42 true null])
    (def int_tuple [1 2 3])
    (def string_atom_tuple ["hello" :world])
    (def nested_tuple [[1 2] [3 4]])
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let result = inferer.process_program(&program).unwrap();
    
    // Check the forms are correctly parsed
    assert_eq!(result.forms.len(), 4);
    
    // Check that mixed_tuple is a tuple with 3 elements
    if let TopLevelKind::VarDef { name, value } = &result.forms[0].node {
        assert_eq!(name, "mixed_tuple");
        if let ExprKind::Literal(Literal::Tuple(elements)) = &value.node {
            assert_eq!(elements.len(), 3);
            
            // First element should be integer 42
            if let ExprKind::Literal(Literal::Integer(val)) = &elements[0].node {
                assert_eq!(*val, 42);
            } else {
                panic!("Expected Integer literal, got {:?}", elements[0].node);
            }
            
            // Second element should be boolean true
            if let ExprKind::Literal(Literal::Boolean(val)) = &elements[1].node {
                assert_eq!(*val, true);
            } else {
                panic!("Expected Boolean literal, got {:?}", elements[1].node);
            }
            
            // Third element should be null
            if let ExprKind::Literal(Literal::Null) = &elements[2].node {
                // Pass
            } else {
                panic!("Expected Null literal, got {:?}", elements[2].node);
            }
        } else {
            panic!("Expected Tuple literal, got {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got {:?}", result.forms[0].node);
    }
    
    // Check that int_tuple is a tuple with 3 integer elements
    if let TopLevelKind::VarDef { name, value } = &result.forms[1].node {
        assert_eq!(name, "int_tuple");
        if let ExprKind::Literal(Literal::Tuple(elements)) = &value.node {
            assert_eq!(elements.len(), 3);
            
            for (i, element) in elements.iter().enumerate() {
                if let ExprKind::Literal(Literal::Integer(val)) = &element.node {
                    assert_eq!(*val, (i + 1) as i128);
                } else {
                    panic!("Expected Integer literal, got {:?}", element.node);
                }
            }
        } else {
            panic!("Expected Tuple literal, got {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got {:?}", result.forms[1].node);
    }
    
    // Check that string_atom_tuple is a tuple with a string and an atom
    if let TopLevelKind::VarDef { name, value } = &result.forms[2].node {
        assert_eq!(name, "string_atom_tuple");
        if let ExprKind::Literal(Literal::Tuple(elements)) = &value.node {
            assert_eq!(elements.len(), 2);
            
            // First element should be string "hello"
            if let ExprKind::Literal(Literal::String(val)) = &elements[0].node {
                assert_eq!(val, "hello");
            } else {
                panic!("Expected String literal, got {:?}", elements[0].node);
            }
            
            // Second element should be atom :world
            if let ExprKind::Literal(Literal::Atom(val)) = &elements[1].node {
                assert_eq!(val, "world");
            } else {
                panic!("Expected Atom literal, got {:?}", elements[1].node);
            }
        } else {
            panic!("Expected Tuple literal, got {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got {:?}", result.forms[2].node);
    }
    
    // Check that nested_tuple is a tuple of tuples
    if let TopLevelKind::VarDef { name, value } = &result.forms[3].node {
        assert_eq!(name, "nested_tuple");
        if let ExprKind::Literal(Literal::Tuple(elements)) = &value.node {
            assert_eq!(elements.len(), 2);
            
            // Check first inner tuple [1 2]
            if let ExprKind::Literal(Literal::Tuple(inner_elements)) = &elements[0].node {
                assert_eq!(inner_elements.len(), 2);
                
                if let ExprKind::Literal(Literal::Integer(val1)) = &inner_elements[0].node {
                    assert_eq!(*val1, 1);
                } else {
                    panic!("Expected Integer literal, got {:?}", inner_elements[0].node);
                }
                
                if let ExprKind::Literal(Literal::Integer(val2)) = &inner_elements[1].node {
                    assert_eq!(*val2, 2);
                } else {
                    panic!("Expected Integer literal, got {:?}", inner_elements[1].node);
                }
            } else {
                panic!("Expected Tuple literal, got {:?}", elements[0].node);
            }
            
            // Check second inner tuple [3 4]
            if let ExprKind::Literal(Literal::Tuple(inner_elements)) = &elements[1].node {
                assert_eq!(inner_elements.len(), 2);
                
                if let ExprKind::Literal(Literal::Integer(val1)) = &inner_elements[0].node {
                    assert_eq!(*val1, 3);
                } else {
                    panic!("Expected Integer literal, got {:?}", inner_elements[0].node);
                }
                
                if let ExprKind::Literal(Literal::Integer(val2)) = &inner_elements[1].node {
                    assert_eq!(*val2, 4);
                } else {
                    panic!("Expected Integer literal, got {:?}", inner_elements[1].node);
                }
            } else {
                panic!("Expected Tuple literal, got {:?}", elements[1].node);
            }
        } else {
            panic!("Expected Tuple literal, got {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got {:?}", result.forms[3].node);
    }
}

#[test]
fn test_tuple_literal_inference() {
    let src = r#"
    (def point [10 20 30])
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let typed_program = inferer.process_program(&program).unwrap();
    
    // Check that the tuple type was correctly inferred
    if let TopLevelKind::VarDef { name, value: _ } = &typed_program.forms[0].node {
        assert_eq!(name, "point");
        
        let var_type = inferer.get_variable_type("point").unwrap();
        if let Type::Tuple(element_types) = var_type {
            assert_eq!(element_types.len(), 3);
            assert_eq!(element_types[0], Type::Int(32));
            assert_eq!(element_types[1], Type::Int(32));
            assert_eq!(element_types[2], Type::Int(32));
        } else {
            panic!("Expected Tuple type, got {:?}", var_type);
        }
    } else {
        panic!("Expected VarDef, got {:?}", typed_program.forms[0].node);
    }
}

#[test]
fn test_data_type_inference() {
    let src = r#"
    (type option (data
        [:some i32]
        [:none]
    ))
    (def maybe-value [:some 42])
    (def no-value [:none])
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let typed_program = inferer.process_program(&program).unwrap();
    
    // Check that the option type was correctly defined
    if let TopLevelKind::TypeDef { name, ty } = &typed_program.forms[0].node {
        assert_eq!(name, "option");
        
        if let Type::Data(variants) = ty {
            assert_eq!(variants.len(), 2);
            
            // :some variant should have an i32 value
            assert_eq!(variants[0].0, "some");
            assert_eq!(variants[0].1, Some(Type::Int(32)));
            
            // :none variant should have no value
            assert_eq!(variants[1].0, "none");
            assert_eq!(variants[1].1, None);
        } else {
            panic!("Expected Data type, got {:?}", ty);
        }
    } else {
        panic!("Expected TypeDef, got {:?}", typed_program.forms[0].node);
    }
    
    // Check maybe-value is of the correct data type
    if let TopLevelKind::VarDef { name, value: _ } = &typed_program.forms[1].node {
        assert_eq!(name, "maybe-value");
        
        let var_type = inferer.get_variable_type("maybe-value").unwrap();
        match var_type {
            Type::Data(_) => {}, // Correct type
            _ => panic!("Expected Data type, got {:?}", var_type),
        }
    } else {
        panic!("Expected VarDef, got {:?}", typed_program.forms[1].node);
    }
    
    // Check no-value is of the correct data type
    if let TopLevelKind::VarDef { name, value: _ } = &typed_program.forms[2].node {
        assert_eq!(name, "no-value");
        
        let var_type = inferer.get_variable_type("no-value").unwrap();
        match var_type {
            Type::Data(_) => {}, // Correct type
            _ => panic!("Expected Data type, got {:?}", var_type),
        }
    } else {
        panic!("Expected VarDef, got {:?}", typed_program.forms[2].node);
    }
} 