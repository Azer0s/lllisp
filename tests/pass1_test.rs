use lllisp::{
    ast::{Type, TopLevelKind},
    parser::parse_program,
    type_inference::TypeInferer,
    CompilerPass1,
};

#[test]
fn test_monomorphize_generic_type() {
    // Create a test program with generic type definitions and instantiations
    let src = r#"
    ;; Define a generic vector type
    (type vector<t> (struct
        (:data (ptr t))
        (:len i32)
        (:cap i32)
    ))
    
    ;; Define a generic option type
    (type option<t> (data
        [:some t]
        [:none]
    ))
    
    ;; Use concrete instantiations of the generic types
    (def int_vec (vector<i32> null 0 0))
    (def float_vec (vector<f32> null 0 0))
    (def some_value (option<i32> :some 42))
    "#;
    
    // Parse the program
    println!("Parsing program...");
    let program = parse_program(src).unwrap();
    
    // Run type inference
    println!("Running type inference...");
    let mut inferer = TypeInferer::new();
    let typed_program = match inferer.process_program(&program) {
        Ok(program) => program,
        Err(errors) => {
            for err in &errors {
                eprintln!("Type error: {:?}", err);
            }
            panic!("Type inference failed with {} errors", errors.len());
        }
    };
    
    // Run the first compiler pass
    println!("Running first compiler pass...");
    let mut compiler_pass = CompilerPass1::new();
    let processed_program = compiler_pass.process(&typed_program);
    
    // Print all type definitions in the processed program
    println!("Monomorphized types:");
    for form in processed_program.forms {
        if let TopLevelKind::TypeDef { name, ty } = &form.node {
            println!("Type '{}':", name);
            println!("  {:?}", ty);
        }
    }
    
    // The test passes by default, we just want to see the output
    // In a real test, we'd assert that specific types were created correctly
} 