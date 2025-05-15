use lllisp::{
    parser::{parse_program},
    type_inference::{TypeInferer},
};

#[test]
fn test_basic_pattern_matching() {
    let src = r#"
    (type status atom)
    (type result [status i32])
    
    (def match-status (fn [(:r result)] i32
        (match (nth r 0)
            [:ok] (nth r 1)
            [:error] 0)))
    
    (def success (result :ok 42))
    (def failure (result :error 0))
    
    (def success-value (match-status success))
    (def failure-value (match-status failure))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Basic pattern matching should type check");
}

#[test]
fn test_nested_pattern_matching() {
    let src = r#"
    (type status atom)
    (type inner-result [status i32])
    (type result [status inner-result])
    
    (def deep-match (fn [(:r result)] i32
        (match (nth r 0)
            [:ok] (match (nth (nth r 1) 0)
                      [:ok] (nth (nth r 1) 1)
                      [:error] 0)
            [:error] -1)))
    
    (def inner-success (inner-result :ok 42))
    (def inner-failure (inner-result :error 0))
    
    (def success (result :ok inner-success))
    (def partial-failure (result :ok inner-failure))
    (def complete-failure (result :error inner-success))
    
    (def success-value (deep-match success))
    (def partial-failure-value (deep-match partial-failure))
    (def complete-failure-value (deep-match complete-failure))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Nested pattern matching should type check");
}

#[test]
fn test_array_destructuring() {
    let src = r#"
    (type point [i32 i32 i32])
    
    (def extract-x (fn [(:p point)] i32 (nth p 0)))
    (def extract-y (fn [(:p point)] i32 (nth p 1)))
    (def extract-z (fn [(:p point)] i32 (nth p 2)))
    
    (def sum-point (fn [(:p point)] i32
        (+ (+ (nth p 0) (nth p 1)) (nth p 2))))
    
    (def origin (point 0 0 0))
    (def point1 (point 1 2 3))
    
    (def x (extract-x point1))
    (def y (extract-y point1))
    (def z (extract-z point1))
    (def sum (sum-point point1))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Array destructuring should type check");
}

#[test]
fn test_atom_dispatching() {
    let src = r#"
    (type shape-type atom)
    (type circle [shape-type i32])  // type, radius
    (type rectangle [shape-type i32 i32])  // type, width, height
    
    (def create-circle (fn [(:radius i32)] circle
        (circle :circle radius)))
    
    (def create-rectangle (fn [(:width i32) (:height i32)] rectangle
        (rectangle :rectangle width height)))
    
    (def area (fn [(:shape shape-type) (:args i32)] i32
        (match shape
            [:circle] (* 3 (* args args))  // π*r² approximated
            [:rectangle] (* (nth args 0) (nth args 1))
            [_] 0)))
    
    (def circ (create-circle 5))
    (def rect (create-rectangle 4 6))
    
    (def circle-area (area (nth circ 0) (nth circ 1)))
    (def rect-area (area (nth rect 0) (list (nth rect 1) (nth rect 2))))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Atom-based dispatching should type check");
}

#[test]
fn test_complex_pattern_matching() {
    let src = r#"
    (type node-type atom)
    (type tree [node-type i32 tree tree])  // type, value, left, right
    (type leaf [node-type i32])  // type, value
    
    (def create-leaf (fn [(:value i32)] leaf
        (leaf :leaf value)))
    
    (def create-node (fn [(:value i32) (:left tree) (:right tree)] tree
        (tree :node value left right)))
    
    (def sum-tree (fn [(:t tree)] i32
        (match (nth t 0)
            [:leaf] (nth t 1)
            [:node] (+ (nth t 1) 
                       (+ (sum-tree (nth t 2)) 
                          (sum-tree (nth t 3))))
            [_] 0)))
    
    (def leaf1 (create-leaf 1))
    (def leaf2 (create-leaf 2))
    (def leaf3 (create-leaf 3))
    
    (def node1 (create-node 4 leaf1 leaf2))
    (def node2 (create-node 5 leaf3 node1))
    
    (def sum (sum-tree node2))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Complex pattern matching should type check");
}

#[test]
fn test_atom_as_enum() {
    let src = r#"
    (type weekday atom)
    
    (def is-weekend (fn [(:day weekday)] bool
        (or (== day :saturday) (== day :sunday))))
    
    (def is-workday (fn [(:day weekday)] bool
        (not (is-weekend day))))
    
    (def monday :monday)
    (def tuesday :tuesday)
    (def wednesday :wednesday)
    (def thursday :thursday)
    (def friday :friday)
    (def saturday :saturday)
    (def sunday :sunday)
    
    (def monday-work (is-workday monday))
    (def saturday-work (is-workday saturday))
    (def sunday-weekend (is-weekend sunday))
    "#;
    
    let program = parse_program(src).unwrap();
    let mut inferer = TypeInferer::new();
    let processed = inferer.process_program(&program);
    
    assert!(processed.is_ok(), "Using atoms as enum values should type check");
} 