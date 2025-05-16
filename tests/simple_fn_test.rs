use lllisp::{
    ast::{TopLevelKind, ExprKind},
    parser::{parse_program},
};

#[test]
fn test_basic_if_expression() {
    // Test a basic if expression
    let src = "(def result (if (> 10 5) 1 0))";
    
    let program = parse_program(src).unwrap();
    
    if let Some(form) = program.forms.first() {
        if let TopLevelKind::VarDef { name, value } = &form.node {
            assert_eq!(name, "result");
            if let ExprKind::If { condition, then_branch, else_branch } = &value.node {
                assert!(else_branch.is_some(), "If should have an else branch");
            } else {
                panic!("Expected If expression, got {:?}", value.node);
            }
        } else {
            panic!("Expected VarDef, got {:?}", form.node);
        }
    } else {
        panic!("No forms parsed");
    }
}

#[test]
fn test_if_with_nested_expressions() {
    // Test an if with more complex expressions
    let src = "(def max-value (if (> a b) (+ a 10) (* b 2)))";
    
    let program = parse_program(src).unwrap();
    
    if let Some(form) = program.forms.first() {
        if let TopLevelKind::VarDef { name, value } = &form.node {
            assert_eq!(name, "max-value");
            if let ExprKind::If { condition, then_branch, else_branch } = &value.node {
                // Check condition is a function call to ">"
                if let ExprKind::Call { name, args } = &condition.node {
                    assert_eq!(name, ">");
                    assert_eq!(args.len(), 2);
                } else {
                    panic!("Expected function call to '>' in condition, got {:?}", condition.node);
                }
                
                // Check then branch is a function call to "+"
                if let ExprKind::Call { name, args } = &then_branch.node {
                    assert_eq!(name, "+");
                    assert_eq!(args.len(), 2);
                } else {
                    panic!("Expected function call to '+' in then branch, got {:?}", then_branch.node);
                }
                
                // Check that the else branch exists and is a function call to "*"
                if let Some(else_expr) = else_branch {
                    if let ExprKind::Call { name, args } = &else_expr.node {
                        assert_eq!(name, "*");
                        assert_eq!(args.len(), 2);
                    } else {
                        panic!("Expected function call to '*' in else branch, got {:?}", else_expr.node);
                    }
                } else {
                    panic!("Expected else branch");
                }
            } else {
                panic!("Expected If expression, got {:?}", value.node);
            }
        } else {
            panic!("Expected VarDef, got {:?}", form.node);
        }
    } else {
        panic!("No forms parsed");
    }
}

#[test]
fn test_if_without_else() {
    // Test an if without an else branch
    let src = "(def result (if (> x 0) x))";
    
    let program = parse_program(src).unwrap();
    
    if let Some(form) = program.forms.first() {
        if let TopLevelKind::VarDef { name, value } = &form.node {
            assert_eq!(name, "result");
            
            // Currently, if without else is parsed as a Call, not an If
            if let ExprKind::Call { name: call_name, args } = &value.node {
                assert_eq!(call_name, "if", "Call name should be 'if'");
                assert_eq!(args.len(), 2, "If without else should have 2 arguments");
                
                // First arg should be the condition
                if let ExprKind::Call { name, args: cond_args } = &args[0].node {
                    assert_eq!(name, ">");
                    assert_eq!(cond_args.len(), 2);
                } else {
                    panic!("Expected function call to '>' in condition, got {:?}", args[0].node);
                }
            } else {
                panic!("Expected Call expression for if without else, got {:?}", value.node);
            }
        } else {
            panic!("Expected VarDef, got {:?}", form.node);
        }
    } else {
        panic!("No forms parsed");
    }
} 