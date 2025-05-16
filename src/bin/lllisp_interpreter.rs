use std::fs;
use std::env;
use std::process;
use lllisp::parser::parse_program;
use lllisp::interpreter::Interpreter;
use lllisp::macro_expander::MacroExpander;
use lllisp::alias_folding::AliasFolding;

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
                    println!("Successfully parsed program with {} top-level forms", program.forms.len());
                    
                    // Apply macro expansion
                    let mut macro_expander = MacroExpander::new();
                    let expanded_program = macro_expander.process_program(&program);
                    println!("Program after macro expansion: {} top-level forms", expanded_program.forms.len());
                    
                    // Apply alias folding
                    let mut alias_folder = AliasFolding::new();
                    let folded_program = alias_folder.process_program(&expanded_program);
                    println!("Program after alias folding: {} top-level forms", folded_program.forms.len());
                    
                    // Run the interpreter on the processed program
                    let mut interpreter = Interpreter::new();
                    match interpreter.eval_program(&folded_program) {
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