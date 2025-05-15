use lllisp::{
    ast::{ExprKind, TopLevelKind},
    parser::{parse_program},
};

#[test]
fn test_if_in_var_def() {
    // Test if condition in a variable definition
    let src = "(def max-value (if (> a b) a b))";
    
    let program = parse_program(src).unwrap();
    assert_eq!(program.forms.len(), 1, "Program should have one form");
    
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "max-value");
        if let ExprKind::If { condition, then_branch, else_branch } = &value.node {
            // Verify condition is a binary operation
            if let ExprKind::Binary { .. } = &condition.node {
                assert!(true, "Condition is a binary expression");
            } else {
                panic!("Expected Binary expression, got: {:?}", condition.node);
            }
            
            // Verify else branch exists
            assert!(else_branch.is_some(), "If should have an else branch");
        } else {
            panic!("Expected If expression, got: {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got: {:?}", program.forms[0].node);
    }
}

#[test]
fn test_nested_if_in_var_def() {
    // Test nested if conditions
    let src = "(def result (if (> x 0) (if (> x 10) 2 1) 0))";
    
    let program = parse_program(src).unwrap();
    assert_eq!(program.forms.len(), 1, "Program should have one form");
    
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "result");
        if let ExprKind::If { condition: _, then_branch, else_branch: _ } = &value.node {
            // Check that the then_branch is also an if expression
            if let ExprKind::If { .. } = &then_branch.node {
                assert!(true, "Found nested if in then branch");
            } else {
                panic!("Expected If expression in then branch, got: {:?}", then_branch.node);
            }
        } else {
            panic!("Expected If expression, got: {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got: {:?}", program.forms[0].node);
    }
}

#[test]
fn test_if_with_do_blocks() {
    // Test if with do blocks
    let src = "(def result (if (> x 0) (do (def y (+ x 1)) (* y 2)) (do (def z (- 0 x)) (* z 2))))";
    
    let program = parse_program(src).unwrap();
    assert_eq!(program.forms.len(), 1, "Program should have one form");
    
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "result");
        if let ExprKind::If { condition: _, then_branch, else_branch } = &value.node {
            // Check that both branches are do blocks
            if let ExprKind::Do(expressions) = &then_branch.node {
                assert_eq!(expressions.len(), 2, "Then branch should have 2 expressions");
            } else {
                panic!("Expected Do block in then branch, got: {:?}", then_branch.node);
            }
            
            if let Some(else_expr) = else_branch {
                if let ExprKind::Do(expressions) = &else_expr.node {
                    assert_eq!(expressions.len(), 2, "Else branch should have 2 expressions");
                } else {
                    panic!("Expected Do block in else branch, got: {:?}", else_expr.node);
                }
            } else {
                panic!("Expected else branch");
            }
        } else {
            panic!("Expected If expression, got: {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got: {:?}", program.forms[0].node);
    }
}

#[test]
fn test_if_without_else() {
    // Test if without else branch
    let src = "(def result (if (> x 0) x))";
    
    let program = parse_program(src).unwrap();
    assert_eq!(program.forms.len(), 1, "Program should have one form");
    
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "result");
        
        // Check if parser handles it as a Call or an If
        match &value.node {
            ExprKind::Call { name: if_name, args } if if_name == "if" => {
                assert_eq!(args.len(), 2, "If without else should have 2 arguments");
            },
            ExprKind::If { condition: _, then_branch: _, else_branch } => {
                assert!(else_branch.is_none(), "If should not have an else branch");
            },
            _ => {
                panic!("Expected If expression or Call with 'if', got: {:?}", value.node);
            }
        }
    } else {
        panic!("Expected VarDef, got: {:?}", program.forms[0].node);
    }
} 