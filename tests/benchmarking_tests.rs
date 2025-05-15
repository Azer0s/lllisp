use lllisp::{
    parser::{parse_program},
    type_inference::{TypeInferer},
};
use std::time::{Instant};

#[test]
fn test_large_atom_program() {
    // Generate a program with many atom literals and types
    let mut src = String::from("
    (type status atom)
    (type color atom)
    (type shape atom)
    (type size atom)
    (type direction atom)
    ");
    
    // Add 100 atom declarations
    for i in 0..100 {
        src.push_str(&format!("(def atom{} :atom{})\n", i, i));
    }
    
    let start = Instant::now();
    let program = parse_program(&src).unwrap();
    let parse_duration = start.elapsed();
    
    let start = Instant::now();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    let infer_duration = start.elapsed();
    
    assert!(processed.is_ok(), "Large atom program should type check");
    
    // We're not asserting specific times, but the operations should complete
    // in a reasonable amount of time
    println!("Large atom program parse time: {:?}", parse_duration);
    println!("Large atom program inference time: {:?}", infer_duration);
}

#[test]
fn test_large_array_program() {
    // Generate a program with deeply nested array types
    let mut src = String::from("
    (type i32-array [i32, 10])
    ");
    
    // Add 10 levels of nested arrays
    let mut current_type = "i32-array".to_string();
    for i in 0..10 {
        let new_type = format!("array{}", i);
        src.push_str(&format!("(type {} [{}, 5])\n", new_type, current_type));
        current_type = new_type;
    }
    
    // Add some variable declarations
    src.push_str(&format!("(def deep-array ({}))\n", current_type));
    
    let start = Instant::now();
    let program = parse_program(&src).unwrap();
    let parse_duration = start.elapsed();
    
    let start = Instant::now();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    let infer_duration = start.elapsed();
    
    assert!(processed.is_ok(), "Large nested array program should type check");
    
    println!("Large array program parse time: {:?}", parse_duration);
    println!("Large array program inference time: {:?}", infer_duration);
}

#[test]
fn test_complex_mixed_program() {
    // Generate a program with a mix of atoms and arrays in complex structures
    let mut src = String::from("
    (type status atom)
    (type record [status, i32, string])
    (type records [record, 100])
    ");
    
    // Add 50 record declarations
    for i in 0..50 {
        let status = if i % 3 == 0 { ":ok" } else if i % 3 == 1 { ":error" } else { ":pending" };
        src.push_str(&format!("(def record{} (record {} {} \"record{}\"))\n", 
                             i, status, i * 10, i));
    }
    
    // Add functions to process records
    src.push_str("
    (fn is-ok [r: record] -> bool
        (== (nth r 0) :ok))
    
    (fn get-value [r: record] -> i32
        (nth r 1))
    
    (fn get-name [r: record] -> string
        (nth r 2))
    ");
    
    // Process all records in a loop (simulating)
    for i in 0..50 {
        src.push_str(&format!("(def is-ok{} (is-ok record{}))\n", i, i));
        src.push_str(&format!("(def value{} (get-value record{}))\n", i, i));
        src.push_str(&format!("(def name{} (get-name record{}))\n", i, i));
    }
    
    let start = Instant::now();
    let program = parse_program(&src).unwrap();
    let parse_duration = start.elapsed();
    
    let start = Instant::now();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    let infer_duration = start.elapsed();
    
    assert!(processed.is_ok(), "Complex mixed program should type check");
    
    println!("Complex mixed program parse time: {:?}", parse_duration);
    println!("Complex mixed program inference time: {:?}", infer_duration);
}

#[test]
fn test_array_access_patterns() {
    // Test various array access patterns to ensure they are efficient
    let src = r#"
    (type point [i32, i32, i32])
    (type points [point, 1000])
    
    (fn get-x [p: point] -> i32 (nth p 0))
    (fn get-y [p: point] -> i32 (nth p 1))
    (fn get-z [p: point] -> i32 (nth p 2))
    
    (fn sum-point [p: point] -> i32
        (+ (+ (get-x p) (get-y p)) (get-z p)))
    
    (fn process-points [pts: points] -> i32
        0)  // Simplified for benchmark purposes
    
    (def pts (points))
    (def result (process-points pts))
    "#;
    
    let start = Instant::now();
    let program = parse_program(src).unwrap();
    let parse_duration = start.elapsed();
    
    let start = Instant::now();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    let infer_duration = start.elapsed();
    
    assert!(processed.is_ok(), "Array access patterns should type check");
    
    println!("Array access parse time: {:?}", parse_duration);
    println!("Array access inference time: {:?}", infer_duration);
}

#[test]
fn test_atom_comparison_perf() {
    // Test the performance of atom comparisons
    let mut src = String::from("
    (type status atom)
    (type result [status, i32])
    
    (fn is-ok [r: result] -> bool
        (== (nth r 0) :ok))
    
    (fn is-error [r: result] -> bool
        (== (nth r 0) :error))
    
    (fn is-pending [r: result] -> bool
        (== (nth r 0) :pending))
    ");
    
    // Add many result values
    for i in 0..100 {
        let status = if i % 3 == 0 { ":ok" } else if i % 3 == 1 { ":error" } else { ":pending" };
        src.push_str(&format!("(def r{} (result {} {}))\n", i, status, i));
    }
    
    // Check all results
    for i in 0..100 {
        src.push_str(&format!("(def is-ok{} (is-ok r{}))\n", i, i));
        src.push_str(&format!("(def is-error{} (is-error r{}))\n", i, i));
        src.push_str(&format!("(def is-pending{} (is-pending r{}))\n", i, i));
    }
    
    let start = Instant::now();
    let program = parse_program(&src).unwrap();
    let parse_duration = start.elapsed();
    
    let start = Instant::now();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    let infer_duration = start.elapsed();
    
    assert!(processed.is_ok(), "Atom comparison performance test should type check");
    
    println!("Atom comparison parse time: {:?}", parse_duration);
    println!("Atom comparison inference time: {:?}", infer_duration);
} 