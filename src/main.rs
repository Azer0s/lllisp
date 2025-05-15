use std::fs;
use std::env;
use std::process;
use lllisp::parser;
use lllisp::type_inference::TypeInferer;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        process::exit(1);
    }
    
    let filename = &args[1];
    
    match fs::read_to_string(filename) {
        Ok(src) => {
            match parser::parse_program(&src) {
                Ok(program) => {
                    println!("Successfully parsed program:");
                    
                    // Apply type inference
                    let mut inferer = TypeInferer::new();
                    match inferer.process_program(&program) {
                        Ok(typed_program) => {
                            println!("Successfully type-checked program:");
                            println!("{:#?}", typed_program);
                        },
                        Err(errors) => {
                            eprintln!("Type errors in program:");
                            for error in errors {
                                eprintln!("  {:?}", error);
                            }
                            process::exit(1);
                        }
                    }
                },
                Err(errors) => {
                    eprintln!("Failed to parse program:");
                    for error in errors {
                        eprintln!("  {}", error);
                    }
                    process::exit(1);
                }
            }
        },
        Err(e) => {
            eprintln!("Error reading file {}: {}", filename, e);
            process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lllisp::ast::{Type, ExprKind, Literal, TopLevelKind};
    use lllisp::parser;
    
    #[test]
    fn test_type_aliases() {
        let src = r#"
        (type char u8)
        (type str (ptr char))
        "#;
        
        let program = parser::parse_program(src).unwrap();
        assert_eq!(program.forms.len(), 2);
        
        if let TopLevelKind::TypeDef { name, ty } = &program.forms[0].node {
            assert_eq!(name, "char");
            assert_eq!(ty, &Type::UInt(8));
        } else {
            panic!("Expected TypeDef");
        }
        
        if let TopLevelKind::TypeDef { name, ty } = &program.forms[1].node {
            assert_eq!(name, "str");
            assert_eq!(ty, &Type::Pointer(Box::new(Type::Named("char".to_string()))));
        } else {
            panic!("Expected TypeDef");
        }
        
        // Test type inference
        let mut inferer = TypeInferer::new();
        let typed_program = inferer.process_program(&program).unwrap();
        
        // Type aliases should be unchanged by inference
        if let TopLevelKind::TypeDef { name, ty } = &typed_program.forms[1].node {
            assert_eq!(name, "str");
            assert_eq!(ty, &Type::Pointer(Box::new(Type::Named("char".to_string()))));
        } else {
            panic!("Expected TypeDef");
        }
    }
    
    #[test]
    fn test_variable_declarations() {
        let src = r#"
        (def a (i32 43))
        (def b (ptr i32 (addr a)))
        (def c (f16 1.3))
        "#;
        
        let program = parser::parse_program(src).unwrap();
        // Test type inference
        let mut inferer = TypeInferer::new();
        let typed_program = inferer.process_program(&program).unwrap();
        // Check first variable with explicit type
        if let TopLevelKind::VarDef { name, value } = &typed_program.forms[0].node {
            assert_eq!(name, "a");
            // No TypedExpr variant, so just check for literal
            if let ExprKind::Call { name: call_name, .. } = &value.node {
                assert_eq!(call_name, "i32");
            } else {
                panic!("Expected Call expression for i32, got {:?}", value.node);
            }
        } else {
            panic!("Expected VarDef");
        }
    }
    
    #[test]
    fn test_type_inference() {
        let src = "(def x 42)";
        let program = parser::parse_program(src).unwrap();
        assert_eq!(program.forms.len(), 1, "Program should have exactly one form");
        let mut inferer = TypeInferer::new();
        let typed_program = inferer.process_program(&program).unwrap();
        if let TopLevelKind::VarDef { name, value } = &typed_program.forms[0].node {
            assert_eq!(name, "x");
            // No TypedExpr variant, so just check for literal
            if let ExprKind::Literal(Literal::Integer(val)) = &value.node {
                assert_eq!(*val, 42);
            } else {
                panic!("Expected Integer literal, got {:?}", value.node);
            }
        } else {
            panic!("Expected VarDef");
        }
        // Now test with the second literal type
        let src = "(def y 3.14)";
        let program = parser::parse_program(src).unwrap();
        assert_eq!(program.forms.len(), 1, "Program should have exactly one form");
        let mut inferer = TypeInferer::new();
        let typed_program = inferer.process_program(&program).unwrap();
        if let TopLevelKind::VarDef { name, value } = &typed_program.forms[0].node {
            assert_eq!(name, "y");
            if let ExprKind::Literal(Literal::Float(val)) = &value.node {
                assert_eq!(*val, 3.14);
            } else {
                panic!("Expected Float literal, got {:?}", value.node);
            }
        } else {
            panic!("Expected VarDef");
        }
        // And test with a boolean literal
        let src = "(def z true)";
        let program = parser::parse_program(src).unwrap();
        assert_eq!(program.forms.len(), 1, "Program should have exactly one form");
        let mut inferer = TypeInferer::new();
        let typed_program = inferer.process_program(&program).unwrap();
        if let TopLevelKind::VarDef { name, value } = &typed_program.forms[0].node {
            assert_eq!(name, "z");
            if let ExprKind::Literal(Literal::Boolean(val)) = &value.node {
                assert_eq!(*val, true);
            } else {
                panic!("Expected Boolean literal, got {:?}", value.node);
            }
        } else {
            panic!("Expected VarDef");
        }
    }
} 