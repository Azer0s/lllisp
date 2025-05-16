use lllisp::{
    ast::{ExprKind, TopLevelKind},
    parser::parse_program,
    type_inference::TypeInferer,
    alias_folding::AliasFolding,
};

#[test]
fn test_basic_alias() {
    let src = r#"
    (def stdio (use :header "stdio.h"))
    (alias printf stdio/printf)
    (printf "Hello, %s!" "world")
    "#;
    
    // Parse the program
    println!("Parsing the test program: {}", src);
    let program = parse_program(src).unwrap();
    
    // Debugging: print all forms
    for (i, form) in program.forms.iter().enumerate() {
        println!("Form {}: {:?}", i, form.node);
    }
    
    // Check that we have the variable definition, the alias, and the function call
    assert_eq!(program.forms.len(), 3, "Expected 3 forms (def, alias, call)");
    
    // Check the module import
    if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
        assert_eq!(name, "stdio");
        assert_eq!(path, "stdio.h");
        assert!(is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
    }
    
    // Check the alias definition
    if let TopLevelKind::Alias { name, module, function } = &program.forms[1].node {
        assert_eq!(name, "printf");
        assert_eq!(module, "stdio");
        assert_eq!(function, "printf");
    } else {
        panic!("Expected Alias, got {:?}", program.forms[1].node);
    }
    
    // Check the function call (should be a regular call before aliasing)
    if let TopLevelKind::Expr(expr) = &program.forms[2].node {
        if let ExprKind::Call { name, args } = &expr {
            assert_eq!(name, "printf");
            assert_eq!(args.len(), 2);
        } else {
            panic!("Expected Call, got {:?}", expr);
        }
    } else {
        panic!("Expected Expr, got {:?}", program.forms[2].node);
    }
    
    // Apply alias folding
    let mut alias_folder = AliasFolding::new();
    let folded_program = alias_folder.process_program(&program);
    
    // Debug the folded program
    println!("Folded program forms: {}", folded_program.forms.len());
    for (i, form) in folded_program.forms.iter().enumerate() {
        println!("Form {}: {:?}", i, form.node);
    }
    
    // The folded program should have 2 forms (the alias is removed)
    assert_eq!(folded_program.forms.len(), 2, "Expected 2 forms after folding (def, call)");
    
    // Check the module definition is preserved
    if let TopLevelKind::ModuleImport { name, path, is_header } = &folded_program.forms[0].node {
        assert_eq!(name, "stdio");
        assert_eq!(path, "stdio.h");
        assert!(is_header);
    } else {
        panic!("Expected ModuleImport, got {:?}", folded_program.forms[0].node);
    }
    
    // Check the function call is now a module call
    if let TopLevelKind::Expr(expr) = &folded_program.forms[1].node {
        if let ExprKind::ModuleCall { module, function, args } = expr {
            assert_eq!(module, "stdio");
            assert_eq!(function, "printf");
            assert_eq!(args.len(), 2);
            println!("ModuleCall: {}::{} with {} args", module, function, args.len());
        } else {
            panic!("Expected ModuleCall, got {:?}", expr);
        }
    } else {
        panic!("Expected Expr, got {:?}", folded_program.forms[1].node);
    }
    
    // Apply type inference to ensure it works with the aliased form
    let mut inferer = TypeInferer::new();
    
    // Manually register the module in the symbol table
    println!("Registering module stdio");
    inferer.register_module("stdio", "stdio.h", true);
    
    let result = inferer.process_program(&folded_program);
    
    // Type inference should succeed
    assert!(result.is_ok(), "Type inference failed: {:?}", result.err());
}

#[test]
fn test_nested_alias() {
    let src = r#"
    (def graphics (use "graphics.ll"))
    (alias render-button graphics/ui/button/render)
    (render-button "Click Me" 100 200 150 50)
    "#;
    
    // Parse the program
    let program = parse_program(src).unwrap();
    
    // Check that we have the module import, the alias, and the function call
    assert_eq!(program.forms.len(), 3, "Expected 3 forms (def, alias, call)");
    
    // Apply alias folding
    let mut alias_folder = AliasFolding::new();
    let folded_program = alias_folder.process_program(&program);
    
    // Debug print the folded program
    println!("Folded program forms: {}", folded_program.forms.len());
    for (i, form) in folded_program.forms.iter().enumerate() {
        println!("Form {}: {:?}", i, form.node);
    }
    
    // The folded program should have 2 forms (the alias is removed)
    assert_eq!(folded_program.forms.len(), 2, "Expected 2 forms after folding (def, call)");
    
    // Check the function call is now a module call with the nested path
    if let TopLevelKind::Expr(expr) = &folded_program.forms[1].node {
        if let ExprKind::ModuleCall { module, function, args } = expr {
            // In our parser, we store the full module path in the module field
            assert_eq!(module, "graphics/ui/button");
            assert_eq!(function, "render");
            assert_eq!(args.len(), 5);
        } else {
            panic!("Expected ModuleCall, got {:?}", expr);
        }
    } else {
        panic!("Expected Expr, got {:?}", folded_program.forms[1].node);
    }
    
    // Apply type inference to ensure it works with nested module paths
    let mut inferer = TypeInferer::new();
    
    // Manually register the module in the symbol table
    // We need to register the full module path
    inferer.register_module("graphics", "graphics.ll", false);
    inferer.register_module("graphics/ui", "graphics.ll", false);
    inferer.register_module("graphics/ui/button", "graphics.ll", false);
    
    let result = inferer.process_program(&folded_program);
    
    // Type inference should succeed
    assert!(result.is_ok(), "Type inference failed: {:?}", result.err());
}

#[test]
fn test_sample_program() {
    let src = r#"
    (def stdio (use :header "stdio.h"))
    (alias printf stdio/printf)
    (printf "Hello, %s!" "world")
    "#;
    
    // Parse the program
    let program = parse_program(src).unwrap();
    
    // Apply alias folding
    let mut alias_folder = AliasFolding::new();
    let folded_program = alias_folder.process_program(&program);
    
    // Debug the folded program
    println!("Folded program forms: {}", folded_program.forms.len());
    for (i, form) in folded_program.forms.iter().enumerate() {
        println!("Form {}: {:?}", i, form.node);
    }
    
    // Check the ModuleCall is correctly formed
    if let TopLevelKind::Expr(expr) = &folded_program.forms[1].node {
        if let ExprKind::ModuleCall { module, function, args } = expr {
            println!("ModuleCall: {}::{} with {} args at span {:?}", module, function, args.len(), folded_program.forms[1].span);
        }
    }
    
    // Apply type inference 
    let mut inferer = TypeInferer::new();
    
    // Manually register the module in the symbol table
    println!("Registering module stdio");
    inferer.register_module("stdio", "stdio.h", true);
    
    let result = inferer.process_program(&folded_program);
    
    // Type inference should succeed
    assert!(result.is_ok(), "Type inference failed: {:?}", result.err());
} 