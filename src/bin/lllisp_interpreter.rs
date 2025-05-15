use std::fs;
use std::env;
use std::process;
use lllisp::parser::parse_program;
use lllisp::interpreter::Interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        process::exit(1);
    }
    
    let filename = &args[1];
    
    match fs::read_to_string(filename) {
        Ok(src) => {
            match parse_program(&src) {
                Ok(program) => {
                    println!("Successfully parsed program:");
                    
                    // Run the interpreter
                    let mut interpreter = Interpreter::new();
                    match interpreter.eval_program(&program) {
                        Ok(result) => {
                            println!("Program result: {:?}", result);
                        },
                        Err(error) => {
                            eprintln!("Runtime error: {}", error);
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