use lllisp::{
    ast::{ExprKind, TopLevelKind, ForIterator},
    parser::{parse_program, expr_parser},
};
use chumsky::Parser;

#[test]
fn test_infinite_for_loop() {
    // Test a simple infinite for loop
    let src = "(def forever (for true (out \"Hello world\")))";
    
    let program = parse_program(src).unwrap();
    assert_eq!(program.forms.len(), 1, "Program should have one form");
    
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "forever");
        
        if let ExprKind::For { iterator, body } = &value.node {
            // Should have an iterator (condition)
            if let Some(iter) = iterator {
                if let ForIterator::Condition(condition) = &**iter {
                    // Condition should be a literal "true"
                    if let ExprKind::Literal(lit) = &condition.node {
                        assert!(true, "Condition is a literal");
                    } else {
                        panic!("Expected Literal for condition, got: {:?}", condition.node);
                    }
                } else {
                    panic!("Expected Condition iterator, got: {:?}", iter);
                }
            } else {
                panic!("Expected iterator for infinite loop");
            }
            
            // Body should be a call to "out"
            if let ExprKind::Call { name, args } = &body.node {
                assert_eq!(name, "out", "Loop body should call 'out'");
                assert_eq!(args.len(), 1, "out should have 1 argument");
            } else {
                panic!("Expected Call expression in body, got: {:?}", body.node);
            }
        } else {
            panic!("Expected For expression, got: {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got: {:?}", program.forms[0].node);
    }
}

#[test]
fn test_range_based_for_loop() {
    // Test a range-based for loop with list
    let src = "(def print-strings (for (range x (list \"Hello\" \"World\")) (out x)))";
    
    let program = parse_program(src).unwrap();
    assert_eq!(program.forms.len(), 1, "Program should have one form");
    
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "print-strings");
        
        if let ExprKind::For { iterator, body } = &value.node {
            // Should have a range iterator
            if let Some(iter) = iterator {
                if let ForIterator::Range { var, collection } = &**iter {
                    assert_eq!(var, "x", "Iterator variable should be 'x'");
                    
                    // Collection should be a call to "list"
                    if let ExprKind::Call { name, args } = &collection.node {
                        assert_eq!(name, "list", "Collection should be a list");
                        assert_eq!(args.len(), 2, "List should have 2 arguments");
                    } else {
                        panic!("Expected Call to 'list', got: {:?}", collection.node);
                    }
                } else {
                    panic!("Expected Range iterator, got: {:?}", iter);
                }
            } else {
                panic!("Expected iterator for range-based loop");
            }
            
            // Body should be a call to "out" with x
            if let ExprKind::Call { name, args } = &body.node {
                assert_eq!(name, "out", "Loop body should call 'out'");
                assert_eq!(args.len(), 1, "out should have 1 argument");
                
                if let ExprKind::Symbol(arg_name) = &args[0].node {
                    assert_eq!(arg_name, "x", "Argument to 'out' should be 'x'");
                } else {
                    panic!("Expected Symbol 'x', got: {:?}", args[0].node);
                }
            } else {
                panic!("Expected Call expression in body, got: {:?}", body.node);
            }
        } else {
            panic!("Expected For expression, got: {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got: {:?}", program.forms[0].node);
    }
}

#[test]
fn test_numeric_range_for_loop() {
    // Test a range-based for loop with numeric range
    let src = "(def count-to-ten (for (range x (to 10)) (out x)))";
    
    let program = parse_program(src).unwrap();
    assert_eq!(program.forms.len(), 1, "Program should have one form");
    
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "count-to-ten");
        
        if let ExprKind::For { iterator, body } = &value.node {
            // Should have a range iterator
            if let Some(iter) = iterator {
                if let ForIterator::Range { var, collection } = &**iter {
                    assert_eq!(var, "x", "Iterator variable should be 'x'");
                    
                    // Collection should be a call to "to"
                    if let ExprKind::Call { name, args } = &collection.node {
                        assert_eq!(name, "to", "Collection should be a range");
                        assert_eq!(args.len(), 1, "Range should have 1 argument");
                    } else {
                        panic!("Expected Call to 'to', got: {:?}", collection.node);
                    }
                } else {
                    panic!("Expected Range iterator, got: {:?}", iter);
                }
            } else {
                panic!("Expected iterator for range-based loop");
            }
        } else {
            panic!("Expected For expression, got: {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got: {:?}", program.forms[0].node);
    }
}

#[test]
fn test_for_loop_with_block_body() {
    // Test a for loop with a do block as body
    let src = r#"
    (def process-numbers 
      (for (range i (to 5))
        (do
          (def squared (* i i))
          (out squared)
        )
      ))
    "#;
    
    let program = parse_program(src).unwrap();
    assert_eq!(program.forms.len(), 1, "Program should have one form");
    
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "process-numbers");
        
        if let ExprKind::For { iterator, body } = &value.node {
            // Should have a range iterator
            assert!(iterator.is_some(), "Should have an iterator");
            
            // Body should be a do block
            if let ExprKind::Do(expressions) = &body.node {
                assert_eq!(expressions.len(), 2, "Block should have 2 expressions");
                
                // First expression should be a variable definition
                if let ExprKind::Call { name, .. } = &expressions[0].node {
                    assert_eq!(name, "def", "First expression should be a def");
                } else {
                    panic!("Expected def call, got: {:?}", expressions[0].node);
                }
                
                // Second expression should be a call to "out"
                if let ExprKind::Call { name, .. } = &expressions[1].node {
                    assert_eq!(name, "out", "Second expression should be an out call");
                } else {
                    panic!("Expected out call, got: {:?}", expressions[1].node);
                }
            } else {
                panic!("Expected Do block in body, got: {:?}", body.node);
            }
        } else {
            panic!("Expected For expression, got: {:?}", value.node);
        }
    } else {
        panic!("Expected VarDef, got: {:?}", program.forms[0].node);
    }
} 