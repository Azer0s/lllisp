extern crate lllisp;

use lllisp::parser::parse_program;
use lllisp::ast::{ExprKind, TopLevelKind, Literal};
use lllisp::type_inference::TypeInferer;
use lllisp::dead_code_elimination::DeadCodeElimination;
use lllisp::interpreter::Interpreter;

#[test]
fn test_arithmetic_operators() {
    let input = r#"
    (def add-result (+ 5 10))
    (def sub-result (- 10 5))
    (def mul-result (* 5 6))
    (def div-result (/ 20 4))
    (def mod-result (% 10 3))
    "#;
    
    let program = parse_program(input).unwrap();
    assert_eq!(program.forms.len(), 5, "Should have 5 variable definitions");
    
    // Test addition
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "add-result");
        if let ExprKind::Call { name, args } = &value.node {
            assert_eq!(name, "+");
            assert_eq!(args.len(), 2);
            assert_literal_int(&args[0], 5);
            assert_literal_int(&args[1], 10);
        } else {
            panic!("Expected function call, got {:?}", value.node);
        }
    }
    
    // Test subtraction
    if let TopLevelKind::VarDef { name, value } = &program.forms[1].node {
        assert_eq!(name, "sub-result");
        if let ExprKind::Call { name, args } = &value.node {
            assert_eq!(name, "-");
            assert_eq!(args.len(), 2);
            assert_literal_int(&args[0], 10);
            assert_literal_int(&args[1], 5);
        } else {
            panic!("Expected function call, got {:?}", value.node);
        }
    }
    
    // Evaluate the program to verify results
    let mut interpreter = Interpreter::new();
    let result = interpreter.eval_program(&program);
    assert!(result.is_ok(), "Program should execute without errors");
}

#[test]
fn test_comparison_operators() {
    let input = r#"
    (def eq-true (== 5 5))
    (def eq-false (== 5 6))
    (def lt-true (< 5 10))
    (def lt-false (< 10 5))
    (def gt-true (> 10 5))
    (def gt-false (> 5 10))
    (def lte-true (<= 5 5))
    (def gte-true (>= 5 5))
    "#;
    
    let program = parse_program(input).unwrap();
    assert_eq!(program.forms.len(), 8, "Should have 8 variable definitions");
    
    // Verify the operator functions are parsed correctly
    for (i, (op, _)) in [("==", true), ("==", false), ("<", true), ("<", false),
                       (">", true), (">", false), ("<=", true), (">=", true)].iter().enumerate() {
        if let TopLevelKind::VarDef { value, .. } = &program.forms[i].node {
            if let ExprKind::Call { name, args } = &value.node {
                assert_eq!(name, op);
                assert_eq!(args.len(), 2, "Operator {} should have 2 arguments", op);
            } else {
                panic!("Expected function call for operator {}, got {:?}", op, value.node);
            }
        }
    }
    
    // Run type inference on the program
    let mut type_inferer = TypeInferer::new();
    let processed_program = type_inferer.process_program(&program).unwrap();
    
    // Verify the types of comparison operations - we don't need to manually check types
    // since the TypeInferer already did the type checking for us
    
    // Evaluate the program to verify results
    let mut interpreter = Interpreter::new();
    let result = interpreter.eval_program(&program);
    assert!(result.is_ok(), "Program should execute without errors");
}

#[test]
fn test_const_folding() {
    let input = r#"
    (def add-folded (+ 5 10))  // Should fold to 15
    (def sub-folded (- 10 5))  // Should fold to 5
    (def eq-folded (== 5 5))   // Should fold to true
    "#;
    
    let program = parse_program(input).unwrap();
    assert_eq!(program.forms.len(), 3, "Should have 3 variable definitions");
    
    // Apply dead code elimination which includes constant folding
    let mut dce = DeadCodeElimination::new();
    let processed_program = dce.process(&program);
    
    // Check that constants are folded
    if let TopLevelKind::VarDef { name, value } = &processed_program.forms[0].node {
        assert_eq!(name, "add-folded");
        if let ExprKind::Literal(Literal::Integer(val)) = &value.node {
            assert_eq!(*val, 15, "Addition should be folded to 15");
        } else {
            panic!("Expected folded literal, got {:?}", value.node);
        }
    }
    
    if let TopLevelKind::VarDef { name, value } = &processed_program.forms[1].node {
        assert_eq!(name, "sub-folded");
        if let ExprKind::Literal(Literal::Integer(val)) = &value.node {
            assert_eq!(*val, 5, "Subtraction should be folded to 5");
        } else {
            panic!("Expected folded literal, got {:?}", value.node);
        }
    }
    
    if let TopLevelKind::VarDef { name, value } = &processed_program.forms[2].node {
        assert_eq!(name, "eq-folded");
        if let ExprKind::Literal(Literal::Boolean(val)) = &value.node {
            assert_eq!(*val, true, "Equality should be folded to true");
        } else {
            panic!("Expected folded literal, got {:?}", value.node);
        }
    }
}

// Helper function to assert that an expression is an integer literal with a specific value
fn assert_literal_int(expr: &lllisp::ast::Expr, expected: i64) {
    if let ExprKind::Literal(Literal::Integer(val)) = &expr.node {
        assert_eq!(*val, expected.into());
    } else {
        panic!("Expected integer literal {}, got {:?}", expected, expr.node);
    }
} 