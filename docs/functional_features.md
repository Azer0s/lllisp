# Advanced Functional Features in LLLisp

LLLisp supports several advanced functional programming features that allow for powerful abstractions and code reuse. This document describes how to use function types, closures, and currying in LLLisp.

## Function Types

In LLLisp, functions are first-class values, meaning they can be:
- Assigned to variables
- Passed as arguments to functions
- Returned from functions

Function types are specified using the syntax:
```
(fn [param_types] return_type)
```

For example, a function that takes an `i32` and returns an `i32` has type:
```
(fn [i32] i32)
```

A function that takes two `i32` parameters and returns an `i32` has type:
```
(fn [i32 i32] i32)
```

### Function Type Examples

```lisp
;; Define a parameter with a function type
(def apply_to_five (fn [(:f (fn [i32] i32))] i32
  (f 5)
))

;; Pass a function to another function
(def double (fn [(:x i32)] i32 
  (* x 2)
))

(apply_to_five double)  ;; Returns 10
```

## Closures

Closures are functions that capture variables from their defining environment. In LLLisp, when you define a function inside another function, the inner function can access and capture variables from the outer function.

### Closure Example

```lisp
;; A function that returns a closure
(def make_adder (fn [(:n i32)] (fn [i32] i32)
  ;; This returns a closure that captures n
  (fn [(:x i32)] i32
    (+ x n)
  )
))

;; Create closures with different captured values
(def add5 (make_adder 5))
(def add10 (make_adder 10))

(add5 3)   ;; Returns 8
(add10 3)  ;; Returns 13
```

## Currying

Currying is a technique where a function that takes multiple arguments is transformed into a sequence of functions, each taking a single argument. LLLisp supports currying through closures.

### Manual Currying Example

```lisp
;; A curried addition function
(def add_curried (fn [(:x i32)] (fn [i32] i32)
  (fn [(:y i32)] i32
    (+ x y)
  )
))

;; Create specialized adders
(def add5 (add_curried 5))

(add5 3)  ;; Returns 8
```

### Generic Curry Function

```lisp
;; A generic curry function for binary functions
(def curry (fn [(:f (fn [i32 i32] i32))] (fn [i32] (fn [i32] i32))
  (fn [(:x i32)] (fn [i32] i32)
    (fn [(:y i32)] i32
      (f x y)
    )
  )
))

;; Regular binary function
(def add (fn [(:x i32) (:y i32)] i32
  (+ x y)
))

;; Curry it
(def curried_add (curry add))
(def add5 (curried_add 5))

(add5 3)  ;; Returns 8
```

## Higher-Order Functions

Higher-order functions are functions that either take functions as arguments or return functions as results (or both).

### Function Composition Example

```lisp
;; Function composition
(def compose (fn [(:f (fn [i32] i32)) (:g (fn [i32] i32))] (fn [i32] i32)
  (fn [(:x i32)] i32
    (g (f x))
  )
))

(def double (fn [(:x i32)] i32 (* x 2)))
(def square (fn [(:x i32)] i32 (* x x)))

;; Compose functions
(def double_then_square (compose double square))

(double_then_square 3)  ;; Returns 36 ((3*2)^2)
```

## Nested Closures and State Management

LLLisp closures can be nested and can maintain state across function calls:

```lisp
;; A counter generator
(def make_counter (fn [(:start i32)] (fn [] i32)
  (do
    ;; Define a mutable count variable
    (def count start)
    
    ;; Return a closure that captures count and increments it
    (fn [] i32
      (do
        (def old_value count)
        (def count (+ count 1))
        old_value
      )
    )
  )
))

;; Create counters
(def counter1 (make_counter 10))
(def counter2 (make_counter 100))

(counter1)  ;; Returns 10, increments to 11
(counter1)  ;; Returns 11, increments to 12
(counter2)  ;; Returns 100, increments to 101
```

## Implementation Notes

Under the hood, closures in LLLisp are implemented as structs containing:
1. A function pointer to the closure implementation
2. Captured environment variables

This allows for efficient execution while maintaining the captured state for each closure instance.

## Example Files

The `examples` directory contains several files demonstrating these concepts:
- `simple_closures.lllisp`: Basic closure examples
- `complex_closures.lllisp`: More complex closure patterns
- `nested_closures.lllisp`: Nested closures with state management
- `currying.lllisp`: Examples of function currying
- `higher_order.lllisp`: Higher-order function examples 