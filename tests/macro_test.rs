use lllisp::ast::{TopLevelKind, ExprKind, Literal};
use lllisp::parser::parse_program;
use lllisp::macro_expander::MacroExpander;
use lllisp::interpreter::{Interpreter, Value};

#[test]
fn test_simple_macro() {
    // Define a simple program with a macro definition
    let src = r#"
    (def double (macro [x] (+ x x)))
    (double 5)
    "#;
    
    // Parse the program
    let parsed_program = parse_program(src).expect("Failed to parse program");
    
    // Verify the AST structure
    assert_eq!(parsed_program.forms.len(), 2);
    
    // Check the macro definition
    if let TopLevelKind::MacroDef { name, params, body } = &parsed_program.forms[0].node {
        assert_eq!(name, "double");
        assert_eq!(params, &["x".to_string()]);
        
        // Check the macro body (+ x x)
        if let ExprKind::Call { name, args } = &body.node {
            assert_eq!(name, "+");
            assert_eq!(args.len(), 2);
            
            // Both arguments should be the symbol 'x'
            for arg in args {
                if let ExprKind::Symbol(sym_name) = &arg.node {
                    assert_eq!(sym_name, "x");
                } else {
                    panic!("Expected symbol, got {:?}", arg.node);
                }
            }
        } else {
            panic!("Expected Call expression in macro body, got {:?}", body.node);
        }
    } else {
        panic!("Expected MacroDef, got {:?}", parsed_program.forms[0].node);
    }
    
    // Process the program with the macro expander
    let mut expander = MacroExpander::new();
    let expanded_program = expander.process_program(&parsed_program);
    
    // Verify that the macro invocation was expanded
    assert_eq!(expanded_program.forms.len(), 1); // Macro def is removed
    
    // Check the expanded expression (+ 5 5)
    if let TopLevelKind::Expr(expr) = &expanded_program.forms[0].node {
        if let ExprKind::Call { name, args } = expr {
            assert_eq!(name, "+");
            assert_eq!(args.len(), 2);
            
            // Both arguments should be the literal 5
            for arg in args {
                if let ExprKind::Literal(Literal::Integer(val)) = &arg.node {
                    assert_eq!(*val, 5);
                } else {
                    panic!("Expected integer literal, got {:?}", arg.node);
                }
            }
        } else {
            panic!("Expected Call expression, got {:?}", expr);
        }
    } else {
        panic!("Expected Expr, got {:?}", expanded_program.forms[0].node);
    }
}

#[test]
fn test_quasiquote_macro() {
    // Define a program with a macro using quasiquote
    let src = r#"
    (def make-adder (macro [n] `(fn [x] i32 (+ x ~n))))
    (def add-five (make-adder 5))
    "#;
    
    // Parse the program
    let parsed_program = parse_program(src).expect("Failed to parse program");
    
    // Process the program with the macro expander
    let mut expander = MacroExpander::new();
    let expanded_program = expander.process_program(&parsed_program);
    
    // Verify the expansion - the make-adder macro should be removed
    assert_eq!(expanded_program.forms.len(), 1);
    
    // Check that add-five is defined as (fn [x] i32 (+ x 5))
    if let TopLevelKind::VarDef { name, value } = &expanded_program.forms[0].node {
        assert_eq!(name, "add-five");
        
        if let ExprKind::Call { name, args } = &value.node {
            assert_eq!(name, "fn");
            assert_eq!(args.len(), 3);
            
            // First arg should be parameters [x]
            if let ExprKind::Literal(Literal::Tuple(params)) = &args[0].node {
                assert_eq!(params.len(), 1);
                if let ExprKind::Symbol(param_name) = &params[0].node {
                    assert_eq!(param_name, "x");
                } else {
                    panic!("Expected symbol for parameter, got {:?}", params[0].node);
                }
            } else {
                panic!("Expected tuple for parameters, got {:?}", args[0].node);
            }
            
            // Second arg should be return type i32
            if let ExprKind::Symbol(type_name) = &args[1].node {
                assert_eq!(type_name, "i32");
            } else {
                panic!("Expected symbol for return type, got {:?}", args[1].node);
            }
            
            // Third arg should be body (+ x 5)
            if let ExprKind::Call { name, args: body_args } = &args[2].node {
                assert_eq!(name, "+");
                assert_eq!(body_args.len(), 2);
                
                // First arg should be x
                if let ExprKind::Symbol(arg_name) = &body_args[0].node {
                    assert_eq!(arg_name, "x");
                } else {
                    panic!("Expected symbol for first argument, got {:?}", body_args[0].node);
                }
                
                // Second arg should be 5
                if let ExprKind::Literal(Literal::Integer(val)) = &body_args[1].node {
                    assert_eq!(*val, 5);
                } else {
                    panic!("Expected integer literal for second argument, got {:?}", body_args[1].node);
                }
            } else {
                panic!("Expected Call expression for body, got {:?}", args[2].node);
            }
        } else {
            panic!("Expected Call expression, got {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got {:?}", expanded_program.forms[0].node);
    }
}

#[test]
fn test_rest_args_macro() {
    // Define a program with a macro using rest arguments
    let src = r#"
    (def list (macro [& items] `[~@items]))
    (def my-list (list 1 2 3))
    "#;
    
    // Parse the program
    let parsed_program = parse_program(src).expect("Failed to parse program");
    
    // Process the program with the macro expander
    let mut expander = MacroExpander::new();
    let expanded_program = expander.process_program(&parsed_program);
    
    // Verify that the macro invocation was expanded
    assert_eq!(expanded_program.forms.len(), 1);
    
    // Check that my-list is defined as [1 2 3]
    if let TopLevelKind::VarDef { name, value } = &expanded_program.forms[0].node {
        assert_eq!(name, "my-list");
        
        if let ExprKind::Literal(Literal::Tuple(items)) = &value.node {
            assert_eq!(items.len(), 3);
            
            // Check each item in the tuple
            for (i, item) in items.iter().enumerate() {
                if let ExprKind::Literal(Literal::Integer(val)) = &item.node {
                    assert_eq!(*val, (i + 1) as i128);
                } else {
                    panic!("Expected integer literal, got {:?}", item.node);
                }
            }
        } else {
            panic!("Expected tuple literal, got {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got {:?}", expanded_program.forms[0].node);
    }
}

#[test]
fn test_parse_macro_definition() {
    // Define a simple program with a macro definition
    let src = "(def double (macro [x] (+ x x)))";
    
    // Parse the program using the specific parser for macro definitions
    let def = lllisp::parser::parse_macro_definition(src).expect("Failed to parse macro definition");
    
    // Check that it's a macro definition
    match &def.node {
        TopLevelKind::MacroDef { name, params, body } => {
            // Check the macro name
            assert_eq!(name, "double");
            
            // Check it has one parameter
            assert_eq!(params.len(), 1);
            assert_eq!(params[0], "x");
            
            // Check the body is (+ x x)
            if let ExprKind::Call { name, args } = &body.node {
                assert_eq!(name, "+");
                assert_eq!(args.len(), 2);
                
                if let ExprKind::Symbol(sym1) = &args[0].node {
                    assert_eq!(sym1, "x");
                } else {
                    panic!("Expected symbol 'x', got {:?}", args[0].node);
                }
                
                if let ExprKind::Symbol(sym2) = &args[1].node {
                    assert_eq!(sym2, "x");
                } else {
                    panic!("Expected symbol 'x', got {:?}", args[1].node);
                }
            } else {
                panic!("Expected function call, got {:?}", body.node);
            }
        },
        _ => panic!("Expected MacroDef, got {:?}", def.node),
    }
}

#[test]
fn test_parse_macro_use() {
    // Define a program with a macro definition and usage
    let src = "(def double (macro [x] (+ x x)))\n(double 5)";
    
    // Parse the program
    let program = parse_program(src).expect("Failed to parse program");
    
    // Verify we got two top-level forms
    assert_eq!(program.forms.len(), 2);
    
    // First form should be the macro definition (already tested above)
    
    // Second form should be the macro invocation as a regular function call
    match &program.forms[1].node {
        TopLevelKind::Expr(expr) => {
            if let ExprKind::Call { name, args } = expr {
                assert_eq!(name, "double");
                assert_eq!(args.len(), 1);
                
                if let ExprKind::Literal(Literal::Integer(n)) = &args[0].node {
                    assert_eq!(*n, 5);
                } else {
                    panic!("Expected integer literal 5, got {:?}", args[0].node);
                }
            } else {
                panic!("Expected function call, got {:?}", expr);
            }
        },
        _ => panic!("Expected Expr, got {:?}", program.forms[1].node),
    }
}

#[test]
fn test_interpreter_macro_expansion() {
    use lllisp::ast::{ExprKind, Literal, Span, Located};
    use lllisp::interpreter::{Interpreter, Value, Environment};
    
    // Create an interpreter
    let mut interpreter = Interpreter::new();
    
    // Define a simple identity macro that just returns its argument
    let macro_body = Located::new(
        ExprKind::Symbol("x".to_string()),
        Span::new(0, 0)
    );
    
    interpreter.define_macro("identity", vec!["x".to_string()], macro_body);
    
    // Create macro arguments - the value 5
    let arg = Located::new(
        ExprKind::Literal(Literal::Integer(5)),
        Span::new(0, 0)
    );
    
    // Apply the macro
    let result = interpreter.apply_macro("identity", &[arg]).expect("Failed to apply macro");
    
    // Check the result - should be 5
    match &result.node {
        ExprKind::Literal(Literal::Integer(val)) => {
            assert_eq!(*val, 5);
        },
        _ => panic!("Expected integer literal, got {:?}", result.node),
    }
} 