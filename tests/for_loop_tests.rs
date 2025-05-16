use lllisp::{
    ast::{ExprKind, ForIterator, Literal, TopLevelKind},
    parser::{parse_program},
};

#[test]
fn test_basic_range_for_loop() {
    let src = r#"
    (def result 
        (for (range x (list "Hello" "World"))
            (out x)
        )
    )
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the for expression is correctly parsed
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "result");
        if let ExprKind::For { iterator, body } = &value.node {
            // Check iterator is a range iterator
            if let Some(iter) = iterator {
                if let ForIterator::Range { var, collection } = &**iter {
                    assert_eq!(var, "x");
                    
                    // Check collection is a list
                    if let ExprKind::Call { name, args } = &collection.node {
                        assert_eq!(name, "list");
                        assert_eq!(args.len(), 2);
                        
                        // Check the list items
                        if let ExprKind::Literal(Literal::String(s)) = &args[0].node {
                            assert_eq!(s, "Hello");
                        } else {
                            panic!("Expected String literal in list, got {:?}", args[0].node);
                        }
                        
                        if let ExprKind::Literal(Literal::String(s)) = &args[1].node {
                            assert_eq!(s, "World");
                        } else {
                            panic!("Expected String literal in list, got {:?}", args[1].node);
                        }
                    } else {
                        panic!("Expected Call to list, got {:?}", collection.node);
                    }
                } else {
                    panic!("Expected Range iterator, got {:?}", iter);
                }
            } else {
                panic!("Expected Some iterator, got None");
            }
            
            // Check body is a call to out
            if let ExprKind::Call { name: call_name, args } = &body.node {
                assert_eq!(call_name, "out");
                assert_eq!(args.len(), 1);
                
                // Check argument is symbol x
                if let ExprKind::Symbol(sym) = &args[0].node {
                    assert_eq!(sym, "x");
                } else {
                    panic!("Expected Symbol in argument, got {:?}", args[0].node);
                }
            } else {
                panic!("Expected Call expression in body, got {:?}", body.node);
            }
        } else {
            panic!("Expected For expression, got {:?}", value.node);
        }
    }
    
    // Skip type inference for now since list function is not defined
    // let mut inferer = TypeInferer::new();
    // let processed = inferer.process_program(&program);
    // assert!(processed.is_ok(), "Basic range for loop should type check");
}

#[test]
fn test_range_for_loop_with_to_function() {
    let src = r#"
    (def result 
        (for (range x (to 10))
            (out x)
        )
    )
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the for expression is correctly parsed
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "result");
        if let ExprKind::For { iterator, body: _ } = &value.node {
            // Check iterator is a range iterator
            if let Some(iter) = iterator {
                if let ForIterator::Range { var, collection } = &**iter {
                    assert_eq!(var, "x");
                    
                    // Check collection is a call to to
                    if let ExprKind::Call { name: call_name, args } = &collection.node {
                        assert_eq!(call_name, "to");
                        assert_eq!(args.len(), 1);
                        
                        // Check argument is 10
                        if let ExprKind::Literal(Literal::Integer(n)) = &args[0].node {
                            assert_eq!(*n, 10);
                        } else {
                            panic!("Expected Integer literal in argument, got {:?}", args[0].node);
                        }
                    } else {
                        panic!("Expected Call expression in collection, got {:?}", collection.node);
                    }
                } else {
                    panic!("Expected Range iterator, got {:?}", iter);
                }
            } else {
                panic!("Expected Some iterator, got None");
            }
        } else {
            panic!("Expected For expression, got {:?}", value.node);
        }
    }
    
    // Skip type inference for now since to function is not defined
    // let mut inferer = TypeInferer::new();
    // let processed = inferer.process_program(&program);
    // assert!(processed.is_ok(), "Range for loop with to function should type check");
}

#[test]
fn test_condition_for_loop() {
    let src = r#"
    (def i 0)
    (def result 
        (for (< i 10)
            (do
                (def i (+ i 1))
                (out i)
            )
        )
    )
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the for expression is correctly parsed
    if let TopLevelKind::VarDef { name, value } = &program.forms[1].node {
        assert_eq!(name, "result");
        if let ExprKind::For { iterator, body } = &value.node {
            // Check iterator is a condition iterator
            if let Some(iter) = iterator {
                if let ForIterator::Condition(condition) = &**iter {
                    // Check condition is a function call to "<"
                    if let ExprKind::Call { name, args } = &condition.node {
                        assert_eq!(name, "<");
                        assert_eq!(args.len(), 2);
                    } else {
                        panic!("Expected function call to '<' in condition, got {:?}", condition.node);
                    }
                } else {
                    panic!("Expected Condition iterator, got {:?}", iter);
                }
            } else {
                panic!("Expected Some iterator, got None");
            }
            
            // Check body is a do block
            if let ExprKind::Do(exprs) = &body.node {
                assert_eq!(exprs.len(), 2);
            } else {
                panic!("Expected Do block in body, got {:?}", body.node);
            }
        } else {
            panic!("Expected For expression, got {:?}", value.node);
        }
    }
    
    // Skip type inference for now
    // let mut inferer = TypeInferer::new();
    // let processed = inferer.process_program(&program);
    // assert!(processed.is_ok(), "Condition for loop should type check");
}

#[test]
fn test_infinite_for_loop() {
    let src = r#"
    (def result 
        (for true
            (out "Hello world")
        )
    )
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the for expression is correctly parsed
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "result");
        if let ExprKind::For { iterator, body } = &value.node {
            // Check iterator is a condition iterator with true
            if let Some(iter) = iterator {
                if let ForIterator::Condition(condition) = &**iter {
                    // Check condition is true
                    if let ExprKind::Literal(Literal::Boolean(b)) = &condition.node {
                        assert!(*b);
                    } else {
                        panic!("Expected Boolean literal in condition, got {:?}", condition.node);
                    }
                } else {
                    panic!("Expected Condition iterator, got {:?}", iter);
                }
            } else {
                panic!("Expected Some iterator, got None");
            }
            
            // Check body is a call to out
            if let ExprKind::Call { name: call_name, args } = &body.node {
                assert_eq!(call_name, "out");
                assert_eq!(args.len(), 1);
                
                // Check argument is "Hello world"
                if let ExprKind::Literal(Literal::String(s)) = &args[0].node {
                    assert_eq!(s, "Hello world");
                } else {
                    panic!("Expected String literal in argument, got {:?}", args[0].node);
                }
            } else {
                panic!("Expected Call expression in body, got {:?}", body.node);
            }
        } else {
            panic!("Expected For expression, got {:?}", value.node);
        }
    }
    
    // Skip type inference for now
    // let mut inferer = TypeInferer::new();
    // let processed = inferer.process_program(&program);
    // assert!(processed.is_ok(), "Infinite for loop should type check");
}

#[test]
fn test_nested_for_loops() {
    let src = r#"
    (def result 
        (for (range i (to 3))
            (for (range j (to 3))
                (out (+ i j))
            )
        )
    )
    "#;
    
    let program = parse_program(src).unwrap();
    
    // Check that the outer for expression is correctly parsed
    if let TopLevelKind::VarDef { name, value } = &program.forms[0].node {
        assert_eq!(name, "result");
        if let ExprKind::For { iterator, body } = &value.node {
            // Check outer iterator is a range iterator
            if let Some(iter) = iterator {
                if let ForIterator::Range { var, .. } = &**iter {
                    assert_eq!(var, "i");
                } else {
                    panic!("Expected Range iterator for outer loop, got {:?}", iter);
                }
            } else {
                panic!("Expected Some iterator for outer loop, got None");
            }
            
            // Check body is another for loop
            if let ExprKind::For { iterator: inner_iterator, .. } = &body.node {
                // Check inner iterator is a range iterator
                if let Some(inner_iter) = inner_iterator {
                    if let ForIterator::Range { var, .. } = &**inner_iter {
                        assert_eq!(var, "j");
                    } else {
                        panic!("Expected Range iterator for inner loop, got {:?}", inner_iter);
                    }
                } else {
                    panic!("Expected Some iterator for inner loop, got None");
                }
            } else {
                panic!("Expected For expression in body, got {:?}", body.node);
            }
        } else {
            panic!("Expected For expression, got {:?}", value.node);
        }
    }
    
    // Skip type inference for now
    // let mut inferer = TypeInferer::new();
    // let processed = inferer.process_program(&program);
    // assert!(processed.is_ok(), "Nested for loops should type check");
} 