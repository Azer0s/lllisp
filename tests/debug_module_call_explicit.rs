use lllisp::parser::parse_module_call;

fn main() {
    // Test the module call parser directly
    let module_call_src = "(math/sqrt 16)";
    println!("Testing direct module call parsing: {}", module_call_src);
    
    match parse_module_call(module_call_src) {
        Ok(expr) => {
            println!("Successfully parsed! Result: {:?}", expr);
        },
        Err(errors) => {
            println!("Error parsing module call: {:?}", errors);
        }
    }
} 