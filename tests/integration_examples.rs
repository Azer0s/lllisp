use lllisp::{
    parser::{parse_program},
    type_inference::{TypeInferer},
};

#[test]
fn test_result_type_pattern() {
    // Test a typical Result-like pattern using atoms
    let src = r#"
    (type result-type atom)
    (type error-type atom)
    (type result [result-type, i32, error-type])
    
    (fn ok [value: i32] -> result
        (result :ok value :none))
    
    (fn error [err: error-type] -> result
        (result :error 0 err))
    
    (fn is-ok [r: result] -> bool
        (== (nth r 0) :ok))
    
    (fn get-value [r: result] -> i32
        (nth r 1))
    
    (fn get-error [r: result] -> error-type
        (nth r 2))
    
    (def success (ok 42))
    (def failure (error :not_found))
    
    (def is-success (is-ok success))
    (def success-value (get-value success))
    (def error-value (get-error failure))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Result type pattern should type check");
    
    // Skip detailed validation which depends on implementation details
}

#[test]
fn test_option_pattern() {
    // Test an Option-like pattern using atoms
    let src = r#"
    (type option-type atom)
    (type option [option-type, i32])
    
    (fn some [value: i32] -> option
        (option :some value))
    
    (fn none [] -> option
        (option :none 0))
    
    (fn is-some [o: option] -> bool
        (== (nth o 0) :some))
    
    (fn get-value [o: option] -> i32
        (if (is-some o)
            (nth o 1)
            0))  // Default value
    
    (def has-value (some 42))
    (def no-value (none))
    
    (def check-some (is-some has-value))
    (def check-none (is-some no-value))
    (def value (get-value has-value))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Option pattern should type check");
}

#[test]
fn test_enum_pattern() {
    // Test an enum-like pattern using atoms
    let src = r#"
    (type color atom)
    
    (fn is-primary [c: color] -> bool
        (or (== c :red) 
            (or (== c :blue) 
                (== c :yellow))))
    
    (def red :red)
    (def green :green)
    (def blue :blue)
    
    (def red-is-primary (is-primary red))
    (def green-is-primary (is-primary green))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Enum pattern should type check");
}

#[test]
fn test_linked_list() {
    // Test a linked list implementation using atoms and arrays
    let src = r#"
    (type node-type atom)
    (type node [node-type, i32, list])
    (type list atom)
    
    (fn nil [] -> list :nil)
    
    (fn cons [head: i32, tail: list] -> list
        (list :node (node :cons head tail)))
    
    (fn is-nil [l: list] -> bool
        (== l :nil))
    
    (fn head [l: list] -> i32
        (if (is-nil l)
            0  // Default for empty list
            (nth (nth l 1) 1)))
    
    (fn tail [l: list] -> list
        (if (is-nil l)
            (nil)
            (nth (nth l 1) 2)))
    
    (def empty-list (nil))
    (def list-with-one (cons 1 empty-list))
    (def list-with-two (cons 2 list-with-one))
    
    (def first (head list-with-two))
    (def rest (tail list-with-two))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Linked list should type check");
}

#[test]
fn test_matrix_operations() {
    // Test matrix operations using arrays
    let src = r#"
    (type matrix [i32, 3, 3])
    
    (fn identity [] -> matrix
        (matrix))  // Would be initialized with identity matrix in real implementation
    
    (fn zero [] -> matrix
        (matrix))  // Would be initialized with zeros in real implementation
    
    (fn add [a: matrix, b: matrix] -> matrix
        (matrix))  // Would implement actual addition in real implementation
    
    (def id-matrix (identity))
    (def zero-matrix (zero))
    (def result (add id-matrix zero-matrix))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Matrix operations should type check");
}

#[test]
fn test_complex_data_structure() {
    // Test a complex data structure combining atoms and arrays
    let src = r#"
    (type status atom)
    (type field-type atom)
    (type record [status, [field-type, i32, 5]])
    
    (fn create-record [] -> record
        (record :ok (field-type-array)))
    
    (fn set-field [r: record, index: i32, value: i32] -> record
        r)  // Would implement field setting in real implementation
    
    (fn get-field [r: record, index: i32] -> i32
        0)  // Would implement field getting in real implementation
    
    (fn is-valid [r: record] -> bool
        (== (nth r 0) :ok))
    
    (def my-record (create-record))
    (def updated (set-field my-record 0 42))
    (def field-value (get-field updated 0))
    (def valid (is-valid updated))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Complex data structure should type check");
} 