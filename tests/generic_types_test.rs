use lllisp::{
    ast::{Type, TopLevelKind},
    parser::{parse_program},
};

#[test]
fn test_generic_struct() {
    let src = r#"
    (type vector<t> (struct<t> 
        (:x t) 
        (:y t)
    ))
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the generic struct is correctly parsed
    if let TopLevelKind::TypeDef { name, ty } = &program.forms[0].node {
        assert_eq!(name, "vector");
        
        if let Type::GenericStruct { type_params, fields } = ty {
            // Check type parameters
            assert_eq!(type_params.len(), 1);
            assert_eq!(type_params[0], "t");
            
            // Check fields
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].0, "x");
            assert_eq!(fields[0].1, Type::Named("t".to_string()));
            assert_eq!(fields[1].0, "y");
            assert_eq!(fields[1].1, Type::Named("t".to_string()));
        } else {
            panic!("Expected GenericStruct, got {:?}", ty);
        }
    } else {
        panic!("Expected TypeDef, got {:?}", program.forms[0].node);
    }
}

#[test]
fn test_generic_data() {
    let src = r#"
    (type option<t> (data<t>
        [:none]
        [:some t]
    ))
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the generic data type is correctly parsed
    if let TopLevelKind::TypeDef { name, ty } = &program.forms[0].node {
        assert_eq!(name, "option");
        
        if let Type::GenericData { type_params, variants } = ty {
            // Check type parameters
            assert_eq!(type_params.len(), 1);
            assert_eq!(type_params[0], "t");
            
            // Check variants
            assert_eq!(variants.len(), 2);
            assert_eq!(variants[0].0, "none");
            assert!(variants[0].1.is_none());
            assert_eq!(variants[1].0, "some");
            assert_eq!(variants[1].1, Some(Type::Named("t".to_string())));
        } else {
            panic!("Expected GenericData, got {:?}", ty);
        }
    } else {
        panic!("Expected TypeDef, got {:?}", program.forms[0].node);
    }
}

#[test]
fn test_generic_tuple() {
    let src = r#"
    (type pair<a b> (tuple<a b> a b))
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the generic tuple type is correctly parsed
    if let TopLevelKind::TypeDef { name, ty } = &program.forms[0].node {
        assert_eq!(name, "pair");
        
        if let Type::GenericTuple { type_params, types } = ty {
            // Check type parameters
            assert_eq!(type_params.len(), 2);
            assert_eq!(type_params[0], "a");
            assert_eq!(type_params[1], "b");
            
            // Check types
            assert_eq!(types.len(), 2);
            assert_eq!(types[0], Type::Named("a".to_string()));
            assert_eq!(types[1], Type::Named("b".to_string()));
        } else {
            panic!("Expected GenericTuple, got {:?}", ty);
        }
    } else {
        panic!("Expected TypeDef, got {:?}", program.forms[0].node);
    }
}

#[test]
fn test_generic_instance() {
    let src = r#"
    (type vector<t> (struct<t> (:x t) (:y t)))
    (def int_vec (vector<i32> 10 20))
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the generic instance is correctly parsed
    if let TopLevelKind::VarDef { name, value } = &program.forms[1].node {
        assert_eq!(name, "int_vec");
        
        // The value should be a call to vector<i32>
        // We can't directly check the generic instance type from the AST
        // since it's represented as a function call in the expression
        // But we can verify it's a Call with the correct name
        match &value.node {
            lllisp::ast::ExprKind::Call { name, args } => {
                assert_eq!(name, "vector<i32>");
                assert_eq!(args.len(), 2);
            },
            _ => panic!("Expected Call, got {:?}", value.node),
        }
    } else {
        panic!("Expected VarDef, got {:?}", program.forms[1].node);
    }
}

#[test]
fn test_multiple_type_params() {
    let src = r#"
    (type result<o e> (data<o e>
        [:ok o]
        [:err e]
    ))
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the generic data type with multiple type parameters is correctly parsed
    if let TopLevelKind::TypeDef { name, ty } = &program.forms[0].node {
        assert_eq!(name, "result");
        
        if let Type::GenericData { type_params, variants } = ty {
            // Check type parameters
            assert_eq!(type_params.len(), 2);
            assert_eq!(type_params[0], "o");
            assert_eq!(type_params[1], "e");
            
            // Check variants
            assert_eq!(variants.len(), 2);
            assert_eq!(variants[0].0, "ok");
            assert_eq!(variants[0].1, Some(Type::Named("o".to_string())));
            assert_eq!(variants[1].0, "err");
            assert_eq!(variants[1].1, Some(Type::Named("e".to_string())));
        } else {
            panic!("Expected GenericData, got {:?}", ty);
        }
    } else {
        panic!("Expected TypeDef, got {:?}", program.forms[0].node);
    }
} 