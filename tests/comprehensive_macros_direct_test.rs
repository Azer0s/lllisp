use lllisp::ast::{ExprKind, TopLevelKind, Literal, Program, Located, Span, TopLevel};
use lllisp::macro_expander::MacroExpander;
use lllisp::interpreter::{Interpreter, Value};

// Helper function to create simple expression AST nodes
fn expr(kind: ExprKind) -> Located<ExprKind> {
    Located::new(kind, Span::new(0, 0))
}

// Helper function to create simple top-level AST nodes
fn top_level(kind: TopLevelKind) -> TopLevel {
    Located::new(kind, Span::new(0, 0))
}

#[test]
fn test_comprehensive_macros_direct_ast() {
    // Create a program AST directly instead of parsing a file
    let mut program = Program { forms: Vec::new() };

    // Define identity macro: (def identity (macro [x] x))
    let identity_macro = top_level(TopLevelKind::MacroDef { 
        name: "identity".to_string(), 
        params: vec!["x".to_string()], 
        body: expr(ExprKind::Symbol("x".to_string())) 
    });

    // Define double macro: (def double (macro [x] (+ x x)))
    let double_macro = top_level(TopLevelKind::MacroDef { 
        name: "double".to_string(), 
        params: vec!["x".to_string()], 
        body: expr(ExprKind::Call { 
            name: "+".to_string(), 
            args: vec![
                expr(ExprKind::Symbol("x".to_string())), 
                expr(ExprKind::Symbol("x".to_string()))
            ] 
        }) 
    });

    // Define mult macro: (def mult (macro [x y] (* x y)))
    let mult_macro = top_level(TopLevelKind::MacroDef { 
        name: "mult".to_string(), 
        params: vec!["x".to_string(), "y".to_string()], 
        body: expr(ExprKind::Call { 
            name: "*".to_string(), 
            args: vec![
                expr(ExprKind::Symbol("x".to_string())), 
                expr(ExprKind::Symbol("y".to_string()))
            ] 
        }) 
    });

    // Define list macro: (def list (macro [& items] items))
    let list_macro = top_level(TopLevelKind::MacroDef { 
        name: "list".to_string(), 
        params: vec!["&".to_string(), "items".to_string()], 
        body: expr(ExprKind::Symbol("items".to_string())) 
    });

    // Add the macro definitions to the program
    program.forms.push(identity_macro);
    program.forms.push(double_macro);
    program.forms.push(mult_macro);
    program.forms.push(list_macro);

    // Add usage of macros
    // (def result1 (identity 42))
    let result1 = top_level(TopLevelKind::VarDef { 
        name: "result1".to_string(), 
        value: expr(ExprKind::Call { 
            name: "identity".to_string(), 
            args: vec![expr(ExprKind::Literal(Literal::Integer(42)))] 
        }) 
    });

    // (def result2 (double 7))
    let result2 = top_level(TopLevelKind::VarDef { 
        name: "result2".to_string(), 
        value: expr(ExprKind::Call { 
            name: "double".to_string(), 
            args: vec![expr(ExprKind::Literal(Literal::Integer(7)))] 
        }) 
    });

    // (def result3 (mult 6 7))
    let result3 = top_level(TopLevelKind::VarDef { 
        name: "result3".to_string(), 
        value: expr(ExprKind::Call { 
            name: "mult".to_string(), 
            args: vec![
                expr(ExprKind::Literal(Literal::Integer(6))),
                expr(ExprKind::Literal(Literal::Integer(7)))
            ] 
        }) 
    });

    // (def result4 (list 1 2 3 4 5))
    let result4 = top_level(TopLevelKind::VarDef { 
        name: "result4".to_string(), 
        value: expr(ExprKind::Call { 
            name: "list".to_string(), 
            args: vec![
                expr(ExprKind::Literal(Literal::Integer(1))),
                expr(ExprKind::Literal(Literal::Integer(2))),
                expr(ExprKind::Literal(Literal::Integer(3))),
                expr(ExprKind::Literal(Literal::Integer(4))),
                expr(ExprKind::Literal(Literal::Integer(5)))
            ] 
        }) 
    });

    // Add the usage to the program
    program.forms.push(result1);
    program.forms.push(result2);
    program.forms.push(result3);
    program.forms.push(result4);

    // 1. Test macro expansion
    println!("\n=== MACRO DEFINITIONS ===");
    println!("Identity Macro: params = {:?}, body = {:?}", vec!["x".to_string()], ExprKind::Symbol("x".to_string()));
    println!("Double Macro: params = {:?}, body = {:?}", vec!["x".to_string()], ExprKind::Call { 
        name: "+".to_string(), 
        args: vec![
            expr(ExprKind::Symbol("x".to_string())), 
            expr(ExprKind::Symbol("x".to_string()))
        ] 
    });
    
    let mut expander = MacroExpander::new();
    let expanded_program = expander.process_program(&program);
    
    println!("\n=== MACRO EXPANSION RESULTS ===");
    println!("Expanded program forms count: {}", expanded_program.forms.len());
    
    // Verify expansion of basic macros
    let mut found_identity = false;
    let mut found_double = false;
    let mut found_mult = false;
    let mut found_list = false;
    
    for form in &expanded_program.forms {
        match &form.node {
            TopLevelKind::VarDef { name, value, .. } => {
                println!("\nChecking var def: {}", name);
                println!("  Value node: {:?}", value.node);
                
                if name == "result1" {
                    // Should be expanded to 42
                    match &value.node {
                        ExprKind::Literal(Literal::Integer(val)) => {
                            assert_eq!(*val, 42);
                            found_identity = true;
                            println!("✓ Found result1 = 42");
                        },
                        _ => panic!("result1 should be 42, got: {:?}", value.node),
                    }
                } else if name == "result2" {
                    // Should be expanded to (+ 7 7)
                    match &value.node {
                        ExprKind::Call { name: op, args } => {
                            assert_eq!(op, "+");
                            assert_eq!(args.len(), 2);
                            println!("  Double macro expanded to (+) with args:");
                            for (i, arg) in args.iter().enumerate() {
                                println!("    Arg {}: {:?}", i, arg.node);
                            }
                            found_double = true;
                            println!("✓ Found result2 = (+ 7 7)");
                        },
                        _ => panic!("result2 should be (+ 7 7), got: {:?}", value.node),
                    }
                } else if name == "result3" {
                    // Should be expanded to (* 6 7)
                    match &value.node {
                        ExprKind::Call { name: op, args } => {
                            assert_eq!(op, "*");
                            assert_eq!(args.len(), 2);
                            println!("  Mult macro expanded to (*) with args:");
                            for (i, arg) in args.iter().enumerate() {
                                println!("    Arg {}: {:?}", i, arg.node);
                            }
                            found_mult = true;
                            println!("✓ Found result3 = (* 6 7)");
                        },
                        _ => panic!("result3 should be (* 6 7), got: {:?}", value.node),
                    }
                } else if name == "result4" {
                    // Should be expanded to a tuple of 5 items
                    match &value.node {
                        ExprKind::Literal(Literal::Tuple(items)) => {
                            assert_eq!(items.len(), 5);
                            println!("  List macro expanded to tuple with {} items:", items.len());
                            for (i, item) in items.iter().enumerate() {
                                println!("    Item {}: {:?}", i, item.node);
                            }
                            found_list = true;
                            println!("✓ Found result4 with 5 items");
                        },
                        _ => panic!("result4 should be a tuple, got: {:?}", value.node),
                    }
                }
            },
            _ => {}
        }
    }
    
    assert!(found_identity, "identity macro expansion not found");
    assert!(found_double, "double macro expansion not found");
    assert!(found_mult, "mult macro expansion not found");
    assert!(found_list, "list macro expansion not found");
    
    // 2. Test interpretation
    println!("\n=== TESTING INTERPRETATION ===");
    let mut interpreter = Interpreter::new();
    
    // Execute the expanded program
    for form in &expanded_program.forms {
        println!("Evaluating form: {:?}", form.node);
        match interpreter.eval_top_level(form) {
            Ok(value) => println!("  -> Result: {:?}", value),
            Err(e) => println!("  -> Error: {}", e),
        }
    }
    
    // Verify results
    println!("\n=== VERIFICATION RESULTS ===");
    // Check identity macro result
    let result1 = interpreter.get_var("result1")
        .expect("result1 not found");
    match result1 {
        Value::Integer(val) => {
            println!("Identity result: {} (expected 42)", val);
            assert_eq!(val, 42);
        },
        _ => panic!("result1 should be an integer, got: {:?}", result1),
    }
    
    // Check double macro result
    let result2 = interpreter.get_var("result2")
        .expect("result2 not found");
    match result2 {
        Value::Integer(val) => {
            println!("Double result: {} (expected 7 + 7 = 14)", val);
            assert_eq!(val, 14); // 7 + 7 = 14
        },
        _ => panic!("result2 should be an integer, got: {:?}", result2),
    }
    
    // Check multiplication macro result
    let result3 = interpreter.get_var("result3")
        .expect("result3 not found");
    match result3 {
        Value::Integer(val) => {
            println!("Multiplication result: {} (expected 6 * 7 = 42)", val);
            assert_eq!(val, 42); // 6 * 7 = 42
        },
        _ => panic!("result3 should be an integer, got: {:?}", result3),
    }
    
    // Check list macro result
    let result4 = interpreter.get_var("result4")
        .expect("result4 not found");
    match result4 {
        Value::Tuple(items) => {
            println!("List result: tuple with {} items (expected 5 items)", items.len());
            assert_eq!(items.len(), 5);
            for (i, item) in items.iter().enumerate() {
                println!("  Item {}: {:?}", i, item);
            }
        },
        _ => panic!("result4 should be a tuple, got: {:?}", result4),
    }
} 