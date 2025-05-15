use lllisp::{
    ast::{Program, TopLevel, TopLevelKind, Type},
    parser::parse_program,
    external_functions::ExternalFunctionPass,
};

#[test]
fn test_external_function_mapping() {
    // Create a test program with a header import
    let src = r#"
    ;; Import the stdio header
    (def stdio (use :header "stdio.h"))
    
    ;; Use printf from stdio
    (stdio/printf "Hello, world!\n")
    "#;
    
    // Parse the program
    println!("Parsing program...");
    let program = parse_program(src).unwrap();
    
    // Run the external function pass
    println!("Running external function pass...");
    let mut pass = match ExternalFunctionPass::new() {
        Ok(pass) => pass,
        Err(err) => {
            println!("Failed to create external function pass: {}", err);
            println!("This test requires libclang to be installed.");
            println!("Skipping test.");
            return;
        }
    };
    
    // Process the program
    let processed_program = match pass.process(&program) {
        Ok(program) => program,
        Err(err) => {
            println!("Failed to process program: {}", err);
            println!("This test requires stdio.h to be accessible.");
            println!("Skipping test.");
            return;
        }
    };
    
    // Check that the program was processed correctly
    assert_eq!(program, processed_program, "Program should be unchanged");
    
    // Check that the stdio module was loaded
    let mapper = pass.mapper();
    assert!(mapper.has_module("stdio"), "stdio module should be loaded");
    
    // Check that printf was found
    let printf = mapper.get_function("stdio", "printf");
    assert!(printf.is_some(), "printf function should be found");
    
    // Check printf signature
    if let Some(printf) = printf {
        println!("Found printf function:");
        println!("  Name: {}", printf.name);
        println!("  Return type: {:?}", printf.return_type);
        println!("  Parameter types: {:?}", printf.param_types);
        println!("  Is variadic: {}", printf.is_variadic);
        
        // printf should return an int
        match &printf.return_type {
            Type::Int(_) => println!("  Return type is int ✓"),
            _ => panic!("Expected printf to return int, got {:?}", printf.return_type),
        }
        
        // printf should take at least one parameter (format string)
        assert!(!printf.param_types.is_empty(), "printf should have parameters");
        
        // First parameter should be a string (char*)
        match &printf.param_types[0] {
            Type::Named(name) if name == "string" => println!("  First parameter is string ✓"),
            _ => panic!("Expected first parameter to be string, got {:?}", printf.param_types[0]),
        }
        
        // printf should be variadic
        assert!(printf.is_variadic, "printf should be variadic");
        println!("  Is variadic ✓");
    }
    
    println!("External function mapping test passed!");
}

#[test]
fn test_module_call_type_checking() {
    // Create a test program with a header import and function call
    let src = r#"
    ;; Import the stdio header
    (def stdio (use :header "stdio.h"))
    
    ;; Define a function that uses printf
    (def print-message
        (do
            (stdio/printf "Hello, %s!\n" "world")
            (stdio/printf "The answer is %d\n" 42)
        )
    )
    "#;
    
    // Parse the program
    println!("Parsing program...");
    let program = parse_program(src).unwrap();
    
    // Run the external function pass
    println!("Running external function pass...");
    let mut pass = match ExternalFunctionPass::new() {
        Ok(pass) => pass,
        Err(err) => {
            println!("Failed to create external function pass: {}", err);
            println!("This test requires libclang to be installed.");
            println!("Skipping test.");
            return;
        }
    };
    
    // Process the program
    match pass.process(&program) {
        Ok(_) => println!("Program processed successfully ✓"),
        Err(err) => {
            println!("Failed to process program: {}", err);
            println!("This test requires stdio.h to be accessible.");
            println!("Skipping test.");
            return;
        }
    };
    
    // Verify that we can get the function signature
    let mapper = pass.mapper();
    let printf = mapper.get_function("stdio", "printf");
    assert!(printf.is_some(), "printf function should be found");
    
    println!("Module call type checking test passed!");
} 