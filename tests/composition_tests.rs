use lllisp::{
    parser::{parse_program},
    type_inference::{TypeInferer},
};

#[test]
fn test_nested_arrays() {
    let src = r#"
    (type point [i32, i32, i32])
    (type points [point, 10])
    (type matrix [points, 10])
    
    (fn get-point [m: matrix, row: i32, col: i32] -> point
        (nth (nth m row) col))
    
    (fn get-x [m: matrix, row: i32, col: i32] -> i32
        (nth (get-point m row col) 0))
    
    (def grid (matrix))
    (def point-at-2-3 (get-point grid 2 3))
    (def x-at-2-3 (get-x grid 2 3))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Nested arrays should type check");
}

#[test]
fn test_atoms_in_complex_structures() {
    let src = r#"
    (type tag atom)
    (type metadata [tag, i32, i32])  // type, created, modified
    (type entry [tag, string, metadata])  // type, content, metadata
    (type collection [tag, [entry, 100]])  // type, entries
    
    (fn is-document [e: entry] -> bool
        (== (nth e 0) :document))
    
    (fn is-image [e: entry] -> bool
        (== (nth e 0) :image))
    
    (fn get-creation-time [e: entry] -> i32
        (nth (nth e 2) 1))
    
    (fn get-document-count [c: collection] -> i32
        42)  // Simplified for test purposes
    
    (def meta (metadata :meta 1000 1100))
    (def doc (entry :document "Content" meta))
    (def img (entry :image "image.png" meta))
    (def coll (collection :collection (entry-array)))
    
    (def is-doc (is-document doc))
    (def is-img (is-image img))
    (def creation-time (get-creation-time doc))
    (def doc-count (get-document-count coll))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Atoms in complex structures should type check");
}

#[test]
fn test_recursive_types() {
    let src = r#"
    (type tag atom)
    (type json-value [tag, json-data])  // type, data
    (type json-data atom)  // Placeholder for different variants
    
    (fn create-null [] -> json-value
        (json-value :null :null))
    
    (fn create-bool [b: bool] -> json-value
        (json-value :bool (if b :true :false)))
    
    (fn create-number [n: i32] -> json-value
        (json-value :number n))
    
    (fn create-string [s: string] -> json-value
        (json-value :string s))
    
    (fn create-array [arr: [json-value, 10]] -> json-value
        (json-value :array arr))
    
    (fn create-object [fields: [string, 10], values: [json-value, 10]] -> json-value
        (json-value :object (json-object fields values)))
    
    (fn is-null [j: json-value] -> bool
        (== (nth j 0) :null))
    
    (fn is-bool [j: json-value] -> bool
        (== (nth j 0) :bool))
    
    (def null-value (create-null))
    (def bool-value (create-bool true))
    (def num-value (create-number 42))
    (def str-value (create-string "hello"))
    
    (def array-items (json-value-array))
    (def array-value (create-array array-items))
    
    (def is-null-check (is-null null-value))
    (def is-bool-check (is-bool bool-value))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Recursive types should type check");
}

#[test]
fn test_array_of_atoms() {
    let src = r#"
    (type status atom)
    (type status-array [status, 5])
    
    (fn all-ok [statuses: status-array] -> bool
        true)  // Simplified for test purposes
    
    (fn any-error [statuses: status-array] -> bool
        false)  // Simplified for test purposes
    
    (def statuses (status-array))
    (def all-ok-check (all-ok statuses))
    (def any-error-check (any-error statuses))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Array of atoms should type check");
}

#[test]
fn test_function_array() {
    let src = r#"
    (type op-type atom)
    (type op [op-type, i32, i32])
    (type op-array [op, 10])
    
    (fn create-add [a: i32, b: i32] -> op
        (op :add a b))
    
    (fn create-sub [a: i32, b: i32] -> op
        (op :sub a b))
    
    (fn create-mul [a: i32, b: i32] -> op
        (op :mul a b))
    
    (fn create-div [a: i32, b: i32] -> op
        (op :div a b))
    
    (fn evaluate [o: op] -> i32
        (match (nth o 0)
            [:add] (+ (nth o 1) (nth o 2))
            [:sub] (- (nth o 1) (nth o 2))
            [:mul] (* (nth o 1) (nth o 2))
            [:div] (/ (nth o 1) (nth o 2))
            [_] 0))
    
    (fn evaluate-all [ops: op-array, count: i32] -> i32
        42)  // Simplified for test purposes
    
    (def add-op (create-add 5 3))
    (def sub-op (create-sub 10 4))
    (def mul-op (create-mul 3 7))
    (def div-op (create-div 20 5))
    
    (def add-result (evaluate add-op))
    (def sub-result (evaluate sub-op))
    (def mul-result (evaluate mul-op))
    (def div-result (evaluate div-op))
    
    (def ops (op-array))
    (def combined-result (evaluate-all ops 4))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Function array should type check");
} 