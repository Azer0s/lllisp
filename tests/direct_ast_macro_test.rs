use lllisp::ast::{ExprKind, TopLevelKind, Literal, Program, Located, Span, TopLevel};
use lllisp::macro_expander::MacroExpander;
use lllisp::interpreter::{Interpreter, Value};

#[test]
fn test_basic_macros_direct_ast() {
    // Helper function to create expression AST nodes
    fn expr(kind: ExprKind) -> Located<ExprKind> {
        Located::new(kind, Span::new(0, 0))
    }

    // Helper function to create top-level AST nodes
    fn top_level(kind: TopLevelKind) -> TopLevel {
        Located::new(kind, Span::new(0, 0))
    }

    // Create macro definitions
    let macro_defs = vec![
        // Identity macro: (def identity (macro [x] x))
        top_level(TopLevelKind::MacroDef {
            name: "identity".to_string(),
            params: vec!["x".to_string()],
            body: expr(ExprKind::Symbol("x".to_string())),
        }),
        
        // Double macro: (def double (macro [x] (+ x x)))
        top_level(TopLevelKind::MacroDef {
            name: "double".to_string(),
            params: vec!["x".to_string()],
            body: expr(ExprKind::Call {
                name: "+".to_string(),
                args: vec![
                    expr(ExprKind::Symbol("x".to_string())),
                    expr(ExprKind::Symbol("x".to_string())),
                ],
            }),
        }),
        
        // Multiplication macro: (def mult (macro [x y] (* x y)))
        top_level(TopLevelKind::MacroDef {
            name: "mult".to_string(),
            params: vec!["x".to_string(), "y".to_string()],
            body: expr(ExprKind::Call {
                name: "*".to_string(),
                args: vec![
                    expr(ExprKind::Symbol("x".to_string())),
                    expr(ExprKind::Symbol("y".to_string())),
                ],
            }),
        }),
    ];

    // Create variable definitions using the macros
    let var_defs = vec![
        // (def id-result (identity 42))
        top_level(TopLevelKind::VarDef {
            name: "id-result".to_string(),
            value: expr(ExprKind::Call {
                name: "identity".to_string(),
                args: vec![expr(ExprKind::Literal(Literal::Integer(42)))],
            }),
        }),
        
        // (def double-result (double 7))
        top_level(TopLevelKind::VarDef {
            name: "double-result".to_string(),
            value: expr(ExprKind::Call {
                name: "double".to_string(),
                args: vec![expr(ExprKind::Literal(Literal::Integer(7)))],
            }),
        }),
        
        // (def mult-result (mult 6 7))
        top_level(TopLevelKind::VarDef {
            name: "mult-result".to_string(),
            value: expr(ExprKind::Call {
                name: "mult".to_string(),
                args: vec![
                    expr(ExprKind::Literal(Literal::Integer(6))),
                    expr(ExprKind::Literal(Literal::Integer(7))),
                ],
            }),
        }),
    ];

    // Create a program with the macro definitions and variable definitions
    let mut program = Program { forms: Vec::new() };
    program.forms.extend(macro_defs);
    program.forms.extend(var_defs);

    // Process the program with the macro expander
    let mut expander = MacroExpander::new();
    let expanded_program = expander.process_program(&program);

    // 1. Test the macro expansion
    let mut found_identity = false;
    let mut found_double = false;
    let mut found_mult = false;

    println!("\n----- EXAMINING EXPANDED FORMS -----");
    for form in &expanded_program.forms {
        match &form.node {
            TopLevelKind::VarDef { name, value } => {
                println!("Checking expanded var def: {}", name);
                println!("  Value node: {:?}", value.node);
                
                if name == "id-result" {
                    // Should be expanded to 42
                    match &value.node {
                        ExprKind::Literal(Literal::Integer(val)) => {
                            assert_eq!(*val, 42);
                            found_identity = true;
                            println!("✓ Found id-result = 42");
                        },
                        _ => panic!("id-result should be 42, got: {:?}", value.node),
                    }
                } else if name == "double-result" {
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
                            println!("✓ Found double-result = (+ 7 7)");
                        },
                        _ => panic!("double-result should be (+ 7 7), got: {:?}", value.node),
                    }
                } else if name == "mult-result" {
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
                            println!("✓ Found mult-result = (* 6 7)");
                        },
                        _ => panic!("mult-result should be (* 6 7), got: {:?}", value.node),
                    }
                }
            },
            _ => {}
        }
    }
    
    assert!(found_identity, "identity macro expansion not found");
    assert!(found_double, "double macro expansion not found");
    assert!(found_mult, "multiplication macro expansion not found");

    // 2. Test the interpretation
    // Create an interpreter (native functions are already registered in the constructor)
    let mut interpreter = Interpreter::new();
    
    println!("\n----- TESTING INTERPRETATION -----");
    // No need to manually add native functions as they are added in the Interpreter::new() method
    
    // Execute the expanded program
    for form in &expanded_program.forms {
        println!("Evaluating form: {:?}", form.node);
        match interpreter.eval_top_level(form) {
            Ok(value) => println!("  -> Result: {:?}", value),
            Err(e) => println!("  -> Error: {}", e),
        }
    }

    // Verify results
    // Check identity macro result
    let id_result = interpreter.get_var("id-result")
        .expect("id-result not found");
    match id_result {
        Value::Integer(val) => {
            println!("id-result = {}", val);
            assert_eq!(val, 42);
        },
        _ => panic!("id-result should be an integer, got: {:?}", id_result),
    }

    // Check double macro result
    let double_result = interpreter.get_var("double-result")
        .expect("double-result not found");
    match double_result {
        Value::Integer(val) => {
            println!("double-result = {} (should be 7 + 7 = 14)", val);
            assert_eq!(val, 14); // 7 + 7 = 14
        },
        _ => panic!("double-result should be an integer, got: {:?}", double_result),
    }

    // Check multiplication macro result
    let mult_result = interpreter.get_var("mult-result")
        .expect("mult-result not found");
    match mult_result {
        Value::Integer(val) => {
            println!("mult-result = {} (should be 6 * 7 = 42)", val);
            assert_eq!(val, 42); // 6 * 7 = 42
        },
        _ => panic!("mult-result should be an integer, got: {:?}", mult_result),
    }
} 