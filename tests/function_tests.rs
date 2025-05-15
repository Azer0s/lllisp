use lllisp::{
    parser::{parse_program},
    type_inference::{TypeInferer},
};

#[test]
fn test_atom_in_function_params() {
    let src = r#"
    (type status atom)
    (def get-status (fn [(:result status)] status result))
    (def success :ok)
    (def result (get-status success))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Function with atom parameter should type check");
}

#[test]
fn test_atom_in_function_return() {
    let src = r#"
    (type status atom)
    (def atom_lib (use "atom.ll"))
    (def result (atom_lib/get_status))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    // This test only cares that type inference succeeds
    assert!(processed.is_ok(), "Module function call returning atom should type check");
}

#[test]
fn test_array_in_function_params() {
    let src = r#"
    (type int-vec [i32, 5])
    (def first-element (fn [(:vec int-vec)] i32 0))
    (def numbers (int-vec))
    (def first (first-element numbers))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Function with array parameter should type check");
}

#[test]
fn test_atom_array_combination() {
    let src = r#"
    (type status atom)
    (type status-array [status, 3])
    
    (def check-statuses (fn [(:statuses status-array)] bool true))
    
    (def ok :ok)
    (def error :error)
    (def pending :pending)
    
    (def statuses (status-array))
    (def result (check-statuses statuses))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Function with array of atoms should type check");
}

#[test]
fn test_function_returning_array() {
    let src = r#"
    (type int-vec [i32, 3])
    (def array_lib (use "array.ll"))
    (def vector (array_lib/make_vector))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Module function call returning array should type check");
}

#[test]
fn test_complex_function_with_atoms_and_arrays() {
    let src = r#"
    (type status atom)
    (type result [status, i32])
    
    (def process-data (fn [(:data i32)] result 
        (if (> data 0)
            (result :ok data)
            (result :error 0))))
    
    (def data 42)
    (def processed (process-data data))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Complex function with atom and array should type check");
} 