use lllisp::{
    ast::{ExprKind, Span},
    parser::{self, expr_parser, parse_program},
};
use chumsky::prelude::*;

#[test]
fn test_parse_if_expression() {
    // Test direct parsing of an if expression without variable definition
    let src = "(if (> 10 5) 10 5)";
    
    // We need to access the expr_parser directly to test
    // For this test, we'll use the public parse_program function and check the AST
    let program = parser::parse_program(
        format!("(def test_var {})", src).as_str()
    ).unwrap();
    
    assert_eq!(program.forms.len(), 1, "Program should have one form");
    
    // Check that we have a variable definition with an if expression
    if let lllisp::ast::TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "test_var");
        
        // The value should be an If expression directly
        if let ExprKind::If { .. } = &value.node {
            // If we get here, the if expression was parsed correctly
            assert!(true);
        } else {
            panic!("Expected If expression, got: {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got: {:?}", program.forms[0].node);
    }
}

#[test]
fn test_parse_if_expression_directly() {
    // Test direct parsing of an if expression using the expression parser
    let src = "(if (> 10 5) 10 5)";
    
    // Parse directly with expr_parser 
    let result = expr_parser().parse(src);
    
    // The result should be Ok and should be an If expression
    assert!(result.is_ok(), "Failed to parse if expression: {:?}", result.err());
    
    // Check if it's correctly an If expression
    let located = result.unwrap();
    match located.node {
        ExprKind::If { .. } => {
            println!("Successfully parsed if expression!");
            // If we get here, the if expression was parsed correctly
            assert!(true);
        },
        _ => {
            panic!("Expected If expression, got: {:?}", located.node);
        }
    }
}

#[test]
fn test_basic_if() {
    let src = r#"
    (if (> x 5) 
        (+ x 1) 
        (- x 1))
    "#;
    
    let program = parse_program(src).unwrap();
    assert_eq!(program.forms.len(), 1);
    
    // Extract the if expression
    if let lllisp::ast::TopLevelKind::Expr(expr_kind) = &program.forms[0].node {
        if let ExprKind::If { condition, then_branch, else_branch } = expr_kind {
            // We expect an else branch in this case
            assert!(else_branch.is_some());
        } else {
            panic!("Expected If expression");
        }
    } else {
        panic!("Expected Expr top-level form");
    }
} 