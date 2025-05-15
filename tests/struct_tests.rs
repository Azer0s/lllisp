use lllisp::{
    ast::{ExprKind, TopLevelKind, Type, Literal},
    parser::{parse_program, parse_form},
};
use chumsky::Parser;

#[test]
fn test_struct_type_definition() {
    // Test a simple struct type definition
    let src = "(type vector (struct (:x f64) (:y f64)))";
    
    let form = parse_form(src).unwrap();
    
    if let TopLevelKind::TypeDef { name, ty } = &form.node {
        assert_eq!(name, "vector");
        
        if let Type::Struct(fields) = &ty {
            assert_eq!(fields.len(), 2);
            
            assert_eq!(fields[0].0, "x");
            assert_eq!(fields[0].1, Type::Float(64));
            
            assert_eq!(fields[1].0, "y");
            assert_eq!(fields[1].1, Type::Float(64));
        } else {
            panic!("Expected Struct type, got: {:?}", ty);
        }
    } else {
        panic!("Expected TypeDef, got: {:?}", form.node);
    }
}

#[test]
fn test_struct_instantiation() {
    // Test struct instantiation
    let src = r#"
    (type point (struct (:x i32) (:y i32)))
    (def p (point 10 20))
    "#;
    
    let program = parse_program(src).unwrap();
    assert_eq!(program.forms.len(), 2);
    
    // Check the variable definition
    if let TopLevelKind::VarDef { name, value } = &program.forms[1].node {
        assert_eq!(name, "p");
        
        if let ExprKind::Call { name, args } = &value.node {
            assert_eq!(name, "point");
            assert_eq!(args.len(), 2);
            
            // Check first argument (x)
            if let ExprKind::Literal(Literal::Integer(x)) = &args[0].node {
                assert_eq!(*x, 10);
            } else {
                panic!("Expected Integer literal for x, got: {:?}", args[0].node);
            }
            
            // Check second argument (y)
            if let ExprKind::Literal(Literal::Integer(y)) = &args[1].node {
                assert_eq!(*y, 20);
            } else {
                panic!("Expected Integer literal for y, got: {:?}", args[1].node);
            }
        } else {
            panic!("Expected Call expression, got: {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got: {:?}", program.forms[1].node);
    }
}

#[test]
fn test_struct_field_access() {
    // Test field access
    let src = "($ :x p)";
    
    let expr = lllisp::parser::expr_parser().parse(src).unwrap();
    
    if let ExprKind::FieldAccess { object, field } = &expr.node {
        assert_eq!(field, "x");
        
        if let ExprKind::Symbol(name) = &object.node {
            assert_eq!(name, "p");
        } else {
            panic!("Expected Symbol, got: {:?}", object.node);
        }
    } else {
        panic!("Expected FieldAccess, got: {:?}", expr.node);
    }
}

#[test]
fn test_struct_field_set() {
    // Test field set
    let src = "($ :x p 42)";
    
    let expr = lllisp::parser::expr_parser().parse(src).unwrap();
    
    if let ExprKind::SetField { object, field, value } = &expr.node {
        assert_eq!(field, "x");
        
        if let ExprKind::Symbol(name) = &object.node {
            assert_eq!(name, "p");
        } else {
            panic!("Expected Symbol, got: {:?}", object.node);
        }
        
        if let ExprKind::Literal(Literal::Integer(val)) = &value.node {
            assert_eq!(*val, 42);
        } else {
            panic!("Expected Integer literal, got: {:?}", value.node);
        }
    } else {
        panic!("Expected SetField, got: {:?}", expr.node);
    }
}

#[test]
fn test_array_index_set() {
    // Test array index set
    let src = "($ [0] arr 99)";
    
    let expr = lllisp::parser::expr_parser().parse(src).unwrap();
    
    if let ExprKind::SetIndex { array, index, value } = &expr.node {
        if let ExprKind::Symbol(name) = &array.node {
            assert_eq!(name, "arr");
        } else {
            panic!("Expected Symbol, got: {:?}", array.node);
        }
        
        if let ExprKind::Literal(Literal::Integer(idx)) = &index.node {
            assert_eq!(*idx, 0);
        } else {
            panic!("Expected Integer literal for index, got: {:?}", index.node);
        }
        
        if let ExprKind::Literal(Literal::Integer(val)) = &value.node {
            assert_eq!(*val, 99);
        } else {
            panic!("Expected Integer literal for value, got: {:?}", value.node);
        }
    } else {
        panic!("Expected SetIndex, got: {:?}", expr.node);
    }
}

#[test]
fn test_memory_set() {
    // Test memory address set
    let src = "($ (addr 47104) 65)";
    
    let expr = lllisp::parser::expr_parser().parse(src).unwrap();
    
    if let ExprKind::SetAddr { addr, value } = &expr.node {
        // The addr expression is parsed as a function call
        if let ExprKind::Call { name, args } = &addr.node {
            assert_eq!(name, "addr");
            assert_eq!(args.len(), 1);
            
            if let ExprKind::Literal(Literal::Integer(address)) = &args[0].node {
                assert_eq!(*address, 47104);
            } else {
                panic!("Expected Integer literal for address, got: {:?}", args[0].node);
            }
        } else {
            panic!("Expected Call expression for addr, got: {:?}", addr.node);
        }
        
        if let ExprKind::Literal(Literal::Integer(val)) = &value.node {
            assert_eq!(*val, 65);
        } else {
            panic!("Expected Integer literal for value, got: {:?}", value.node);
        }
    } else {
        panic!("Expected SetAddr, got: {:?}", expr.node);
    }
}

#[test]
fn test_complex_struct() {
    // Test a more complex struct with nested fields
    let src = r#"
    (type person (struct 
        (:name [i8])
        (:age i32)
        (:address (struct 
            (:street [i8])
            (:city [i8])
            (:zip i32)
        ))
    ))
    "#;
    
    let form = parse_form(src).unwrap();
    
    if let TopLevelKind::TypeDef { name, ty } = &form.node {
        assert_eq!(name, "person");
        
        if let Type::Struct(fields) = &ty {
            assert_eq!(fields.len(), 3);
            
            // Check name field
            assert_eq!(fields[0].0, "name");
            assert!(matches!(fields[0].1, Type::Array(_, _)));
            
            // Check age field
            assert_eq!(fields[1].0, "age");
            assert_eq!(fields[1].1, Type::Int(32));
            
            // Check address field
            assert_eq!(fields[2].0, "address");
            if let Type::Struct(addr_fields) = &fields[2].1 {
                assert_eq!(addr_fields.len(), 3);
                assert_eq!(addr_fields[0].0, "street");
                assert_eq!(addr_fields[1].0, "city");
                assert_eq!(addr_fields[2].0, "zip");
                assert_eq!(addr_fields[2].1, Type::Int(32));
            } else {
                panic!("Expected Struct type for address field, got: {:?}", fields[2].1);
            }
        } else {
            panic!("Expected Struct type, got: {:?}", ty);
        }
    } else {
        panic!("Expected TypeDef, got: {:?}", form.node);
    }
} 