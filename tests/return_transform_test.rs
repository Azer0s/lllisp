use lllisp::{
    ast::{ExprKind, TopLevelKind},
    parser::parse_program,
    type_inference::TypeInferer,
    CompilerPass1,
};

#[test]
fn test_implicit_return_transformation() {
    // Create a test program with blocks that have implicit returns
    let src = r#"
    ;; Define a variable with a block that has an implicit return
    (def block-value 
        (do
            (+ 1 2)  ;; This is a statement
            (* 3 4)  ;; This is the implicit return value
        )
    )
    
    ;; Define a variable with a conditional that has implicit returns
    (def conditional-value
        (if (> 10 5)
            (+ 10 20)  ;; Implicit return in then branch
            (* 5 5)    ;; Implicit return in else branch
        )
    )
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
    
    // Check that all blocks now have explicit returns
    for form in &processed_program.forms {
        if let TopLevelKind::VarDef { name, value } = &form.node {
            println!("Checking variable: {}", name);
            
            match &value.node {
                ExprKind::Do(exprs) => {
                    // Check that the last expression in the block is a return
                    if !exprs.is_empty() {
                        if let ExprKind::Return(_) = &exprs.last().unwrap().node {
                            println!("  Block has explicit return ✓");
                        } else {
                            panic!("Block in '{}' does not have an explicit return", name);
                        }
                    }
                },
                ExprKind::If { then_branch, else_branch, .. } => {
                    // Check that both branches have returns
                    match &then_branch.node {
                        ExprKind::Return(_) => println!("  Then branch has explicit return ✓"),
                        _ => panic!("Then branch in '{}' does not have an explicit return", name),
                    }
                    
                    if let Some(else_expr) = else_branch {
                        match &else_expr.node {
                            ExprKind::Return(_) => println!("  Else branch has explicit return ✓"),
                            _ => panic!("Else branch in '{}' does not have an explicit return", name),
                        }
                    }
                },
                _ => {}
            }
        }
    }
    
    println!("All blocks have explicit returns ✓");
}

#[test]
fn test_nested_implicit_returns() {
    // Create a test program with more complex nested structures
    let src = r#"
    ;; Define a variable with an if inside a do block
    (def nested-if-in-do
        (do
            (+ 1 2)  ;; Statement
            (if (> 10 5)
                (+ 10 20)  ;; Implicit return in then branch
                (* 5 5)    ;; Implicit return in else branch
            )  ;; This if is the implicit return of the do block
        )
    )
    
    ;; Define a variable with nested if expressions
    (def nested-if
        (if (> 10 5)
            (if (< 3 4)
                (+ 1 1)  ;; Deeply nested implicit return
                (+ 2 2)  ;; Deeply nested implicit return
            )
            (+ 3 3)  ;; Implicit return in else branch
        )
    )
    
    ;; Define a variable with a do block inside an if
    (def if-with-do
        (if (> 10 5)
            (do
                (+ 1 2)  ;; Statement
                (+ 3 4)  ;; Implicit return of do block
            )
            (+ 5 6)  ;; Implicit return in else branch
        )
    )
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
    
    // Check that all nested structures have explicit returns
    for form in &processed_program.forms {
        if let TopLevelKind::VarDef { name, value } = &form.node {
            println!("Checking variable: {}", name);
            
            match name.as_str() {
                "nested-if-in-do" => {
                    // Check the outer do block has a return
                    if let ExprKind::Do(exprs) = &value.node {
                        if let ExprKind::Return(inner) = &exprs.last().unwrap().node {
                            // Check that the return contains an if
                            if let ExprKind::If { then_branch, else_branch, .. } = &inner.node {
                                // Check that both branches of the if have returns
                                match &then_branch.node {
                                    ExprKind::Return(_) => println!("  Nested if-then branch has explicit return ✓"),
                                    _ => panic!("Nested if-then branch does not have an explicit return"),
                                }
                                
                                if let Some(else_expr) = else_branch {
                                    match &else_expr.node {
                                        ExprKind::Return(_) => println!("  Nested if-else branch has explicit return ✓"),
                                        _ => panic!("Nested if-else branch does not have an explicit return"),
                                    }
                                }
                            } else {
                                panic!("Expected If inside Return, found {:?}", inner.node);
                            }
                        } else {
                            panic!("Last expression in do block is not a Return");
                        }
                    } else {
                        panic!("Expected Do block, found {:?}", value.node);
                    }
                },
                "nested-if" => {
                    // Check the outer if has returns in both branches
                    if let ExprKind::If { then_branch, else_branch, .. } = &value.node {
                        // Check the then branch (which should contain another if)
                        match &then_branch.node {
                            ExprKind::Return(inner_if) => {
                                if let ExprKind::If { then_branch: nested_then, else_branch: nested_else, .. } = &inner_if.node {
                                    // Check nested if branches
                                    match &nested_then.node {
                                        ExprKind::Return(_) => println!("  Nested if-then-then branch has explicit return ✓"),
                                        _ => panic!("Nested if-then-then branch does not have an explicit return"),
                                    }
                                    
                                    if let Some(nested_else_expr) = nested_else {
                                        match &nested_else_expr.node {
                                            ExprKind::Return(_) => println!("  Nested if-then-else branch has explicit return ✓"),
                                            _ => panic!("Nested if-then-else branch does not have an explicit return"),
                                        }
                                    }
                                } else {
                                    panic!("Expected nested If, found {:?}", inner_if.node);
                                }
                            },
                            _ => panic!("Outer if-then branch does not have an explicit return"),
                        }
                        
                        // Check the else branch
                        if let Some(else_expr) = else_branch {
                            match &else_expr.node {
                                ExprKind::Return(_) => println!("  Outer if-else branch has explicit return ✓"),
                                _ => panic!("Outer if-else branch does not have an explicit return"),
                            }
                        }
                    } else {
                        panic!("Expected If, found {:?}", value.node);
                    }
                },
                "if-with-do" => {
                    // Check the if has returns in both branches
                    if let ExprKind::If { then_branch, else_branch, .. } = &value.node {
                        // Check the then branch (which should contain a do block)
                        match &then_branch.node {
                            ExprKind::Return(inner_do) => {
                                if let ExprKind::Do(exprs) = &inner_do.node {
                                    // Check that the do block has a return as its last expression
                                    if let ExprKind::Return(_) = &exprs.last().unwrap().node {
                                        println!("  Do block inside if-then has explicit return ✓");
                                    } else {
                                        panic!("Do block inside if-then does not have an explicit return");
                                    }
                                } else {
                                    panic!("Expected Do block, found {:?}", inner_do.node);
                                }
                            },
                            _ => panic!("If-then branch does not have an explicit return"),
                        }
                        
                        // Check the else branch
                        if let Some(else_expr) = else_branch {
                            match &else_expr.node {
                                ExprKind::Return(_) => println!("  If-else branch has explicit return ✓"),
                                _ => panic!("If-else branch does not have an explicit return"),
                            }
                        }
                    } else {
                        panic!("Expected If, found {:?}", value.node);
                    }
                },
                _ => {}
            }
        }
    }
    
    println!("All nested structures have explicit returns ✓");
} 