use lllisp::{
    ast::{ExprKind, Literal, Type, TopLevelKind},
    parser::{parse_program, parse_form},
    type_inference::{TypeInferer},
};

#[test]
fn test_atom_type_definition() {
    let src = "(type status atom)";
    let result = parse_form(src).unwrap();
    
    if let TopLevelKind::TypeDef { name, ty } = result.node {
        assert_eq!(name, "status");
        assert_eq!(ty, Type::Atom);
    } else {
        panic!("Expected TypeDef, got {:?}", result.node);
    }
}

#[test]
fn test_atom_literals() {
    // Simple atom literals
    let program = parse_program("(def success :ok)").unwrap();
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "success");
        if let ExprKind::Literal(Literal::Atom(atom_name)) = &value.node {
            assert_eq!(atom_name, "ok");
        } else {
            panic!("Expected atom literal, got {:?}", value.node);
        }
    }
    
    // Atom literals with hyphens
    let program = parse_program("(def status :connection-lost)").unwrap();
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "status");
        if let ExprKind::Literal(Literal::Atom(atom_name)) = &value.node {
            assert_eq!(atom_name, "connection-lost");
        } else {
            panic!("Expected atom literal, got {:?}", value.node);
        }
    }
    
    // Multiple atoms in a program
    let multi_atom_src = r#"
    (def ok :ok)
    (def error :error)
    (def warning :warning)
    "#;
    
    let program = parse_program(multi_atom_src).unwrap();
    assert_eq!(program.forms.len(), 3);
    
    // Extract atom values from the program
    let mut atoms = Vec::new();
    for form in program.forms {
        if let TopLevelKind::VarDef { value, .. } = form.node {
            if let ExprKind::Literal(Literal::Atom(atom_name)) = value.node {
                atoms.push(atom_name);
            }
        }
    }
    
    // Check that we found all three atoms
    assert!(atoms.contains(&"ok".to_string()));
    assert!(atoms.contains(&"error".to_string()));
    assert!(atoms.contains(&"warning".to_string()));
}

#[test]
fn test_array_comma_syntax() {
    // Basic array type with size
    let array_type_src = "(type numbers [i32, 10])";
    let result = parse_form(array_type_src).unwrap();
    
    if let TopLevelKind::TypeDef { name, ty } = result.node {
        assert_eq!(name, "numbers");
        match ty {
            Type::Array(inner_ty, Some(size)) => {
                assert_eq!(*inner_ty, Type::Int(32));
                assert_eq!(size, 10);
            },
            _ => panic!("Expected Array type, got {:?}", ty),
        }
    } else {
        panic!("Expected TypeDef, got {:?}", result.node);
    }
    
    // Array type without size
    let unsized_array_src = "(type numbers [i32])";
    let result = parse_form(unsized_array_src).unwrap();
    
    if let TopLevelKind::TypeDef { name, ty } = result.node {
        assert_eq!(name, "numbers");
        match ty {
            Type::Array(inner_ty, None) => {
                assert_eq!(*inner_ty, Type::Int(32));
            },
            _ => panic!("Expected Array type, got {:?}", ty),
        }
    } else {
        panic!("Expected TypeDef, got {:?}", result.node);
    }
    
    // Nested array types
    let _nested_array_src = "(type matrix [i32, 5, 5])";
    
    // Multiple array definitions in a program
    let multi_array_src = r#"
    (type int-vec [i32, 10])
    (type float-vec [f32, 10])
    (type byte-vec [u8, 256])
    "#;
    
    let program = parse_program(multi_array_src).unwrap();
    assert!(program.forms.len() >= 3);
}

#[test]
fn test_type_inference_for_atoms() {
    let src = r#"
    (type status atom)
    (def result :ok)
    (def error :error)
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program).unwrap();
    
    // Check result variable
    if let TopLevelKind::VarDef { name, value } = &processed.forms[1].node {
        assert_eq!(name, "result");
        if let ExprKind::Literal(Literal::Atom(atom_name)) = &value.node {
            assert_eq!(atom_name, "ok");
        } else {
            panic!("Expected Atom literal, got {:?}", value.node);
        }
    }
    
    // Check error variable
    if let TopLevelKind::VarDef { name, value } = &processed.forms[2].node {
        assert_eq!(name, "error");
        if let ExprKind::Literal(Literal::Atom(atom_name)) = &value.node {
            assert_eq!(atom_name, "error");
        } else {
            panic!("Expected Atom literal, got {:?}", value.node);
        }
    }
}

#[test]
fn test_type_inference_for_arrays() {
    let src = r#"
    (type int-array [i32, 5])
    (type float-array [f32, 10])
    (def array_lib (use "array.ll"))
    (def numbers (array_lib/create_int_array))
    (def points (array_lib/create_float_array))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program).unwrap();
    
    // Check numbers variable (should be module call with int array)
    if let TopLevelKind::VarDef { name, .. } = &processed.forms[3].node {
        assert_eq!(name, "numbers");
    }
    
    // Check points variable (should be module call with float array)
    if let TopLevelKind::VarDef { name, .. } = &processed.forms[4].node {
        assert_eq!(name, "points");
    }
}

#[test]
fn test_complex_atom_expressions() {
    let src = r#"
    (type status atom)
    (def atom_lib (use "atom.ll"))
    (def result (atom_lib/ok_status))
    (def error (atom_lib/error_status))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program).unwrap();
    
    // Check result variable with module call
    if let TopLevelKind::VarDef { name, .. } = &processed.forms[2].node {
        assert_eq!(name, "result");
    }
}

#[test]
fn test_complex_array_expressions() {
    let src = r#"
    (type int-vec [i32, 3])
    (def array_lib (use "array.ll"))
    (def vector (array_lib/create_vector))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program).unwrap();
    
    // Check vector variable with module call
    if let TopLevelKind::VarDef { name, .. } = &processed.forms[2].node {
        assert_eq!(name, "vector");
    }
}

#[test]
fn test_integration() {
    // Test a comprehensive program that uses both atoms and arrays
    let src = r#"
    (type status atom)
    (type int-vec [i32, 5])
    (type float-vec [f32, 10])
    
    (def ok :ok)
    (def error :error)
    (def pending :pending)
    
    (def numbers (int-vec))
    (def result (status :ok))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    // Make sure the program type-checks correctly
    assert!(processed.is_ok());
} 