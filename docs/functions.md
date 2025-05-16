# LLLisp Functions

LLLisp provides first-class functions with support for higher-order functions and closures. This document describes how to define and use functions in LLLisp.

## Function Definition

Functions are defined using the `fn` keyword, followed by a parameter list in square brackets, a return type, and a body:

```lisp
(def function-name (fn [parameters] return-type
  body
))
```

### Simple Function Examples

```lisp
;; Function with no parameters, returning void
(def say-hello (fn [] void
  (stdio/printf "Hello, world!\n")
))

;; Function with one parameter and explicit return type
(def square (fn [(:x i32)] i32
  (* x x)
))

;; Function with multiple parameters
(def add (fn [(:a i32) (:b i32)] i32
  (+ a b)
))
```

## Parameter Specifications

Parameters are specified with type annotations:

```lisp
;; With explicit type annotations
(def max (fn [(:a i32) (:b i32)] i32
  (if (> a b) a b)
))
```

### Optional Parameters and Default Values

LLLisp doesn't have built-in syntax for optional or default parameters.

### Rest Parameters

LLLisp does not directly support rest parameters (variadic functions) in the syntax. For variadic functions, you typically need to use an array or list parameter, or use C interop for true variadic functions.

## Return Values

Functions return the value of the last expression in their body. The `return` keyword can be used for early returns:

```lisp
(def absolute (fn [(:x i32)] i32
  (if (< x 0)
    (return (- 0 x))
    x
  )
))
```

### Multiple Return Values with Tuples

To return multiple values, use a tuple:

```lisp
;; Function returning a tuple of two values
(def divide (fn [(:a i32) (:b i32)] [i32 bool]
  (if (= b 0)
    [0 false]       ; Error case: division by zero
    [(/ a b) true]  ; Success case: quotient and success flag
  )
))

;; Using the result
(def result (divide 10 2))
(def quotient ($ [0] result))   ; Access first tuple element: 5
(def success ($ [1] result))    ; Access second tuple element: true
```

## Higher-Order Functions

LLLisp supports higher-order functions - functions that take other functions as arguments or return functions as results.

### Functions as Arguments

```lisp
;; A higher-order function that applies a function to each element of an array
(def map (fn [(:f (fn [i32] i32)) (:arr (ptr i32)) (:len i32)] (ptr i32)
  (do
    (def result (malloc (* len (sizeof i32))))
    (for (range i len)
      ($ [i] result (f ($ [i] arr)))
    )
    result
  )
))

;; Using the map function
(def double (fn [(:x i32)] i32 (* x 2)))
(def numbers (array i32 [1 2 3 4 5]))
(def doubled (map double numbers 5))  ; [2 4 6 8 10]
```

### Functions Returning Functions

```lisp
;; A function that returns another function
(def make-adder (fn [(:n i32)] (fn [i32] i32)
  (fn [(:x i32)] i32
    (+ x n)
  )
))

;; Using the function factory
(def add-five (make-adder 5))
(add-five 10)  ; Returns 15
```

## Closures

Closures are functions that capture variables from their surrounding scope. In LLLisp, nested functions automatically capture variables:

```lisp
;; A function that creates a counter
(def make-counter (fn [(:start i32)] (fn [] i32)
  (do
    (def count start)  ; This variable will be captured by the closure
    
    (fn [] i32
      (do
        (def old-count count)
        (def count (+ count 1))  ; Modify the captured variable
        old-count  ; Return the old value
      )
    )
  )
))

;; Create two independent counters
(def counter1 (make-counter 0))
(def counter2 (make-counter 100))

(counter1)  ; Returns 0, increments to 1
(counter1)  ; Returns 1, increments to 2
(counter2)  ; Returns 100, increments to 101
```

## Function Composition and Currying

### Function Composition

```lisp
;; Function composition
(def compose (fn [(:f (fn [i32] i32)) (:g (fn [i32] i32))] (fn [i32] i32)
  (fn [(:x i32)] i32
    (g (f x))
  )
))

(def double (fn [(:x i32)] i32 (* x 2)))
(def increment (fn [(:x i32)] i32 (+ x 1)))

;; Create a new function that doubles first, then increments
(def double-then-increment (compose double increment))

(double-then-increment 3)  ; Returns 7: (3*2)+1
```

### Currying

```lisp
;; Manual currying of a binary function
(def curry-add (fn [(:a i32)] (fn [i32] i32)
  (fn [(:b i32)] i32
    (+ a b)
  )
))

;; Usage
(def add-five (curry-add 5))
(add-five 3)  ; Returns 8
```

## Recursion

LLLisp supports recursive functions:

```lisp
;; Recursive factorial function
(def factorial (fn [(:n i32)] i32
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))
  )
))

;; Tail-recursive factorial
(def factorial-tr (fn [(:n i32)] i32
  (def helper (fn [(:n i32) (:acc i32)] i32
    (if (<= n 1)
      acc
      (helper (- n 1) (* acc n))
    )
  ))
  
  (helper n 1)
))
```

## Function Overloading

LLLisp does not support function overloading in the traditional sense. Instead, use different function names or pattern matching/type checking in the function body.

## Anonymous Functions

Anonymous functions (lambdas) are created using the `fn` keyword without binding them to a name:

```lisp
;; An anonymous function passed directly as an argument
(map (fn [(:x i32)] i32 (* x 2)) numbers 5)

;; Using an anonymous function with higher-order functions
(reduce (fn [(:acc i32) (:x i32)] i32 (+ acc x)) numbers 5 0)
```

## Function Aliases

The `alias` keyword allows creating aliases to functions, particularly useful for module functions:

```lisp
;; Import a module
(def stdio (use :header "stdio.h"))

;; Create an alias for the printf function
(alias printf stdio/printf)

;; Now printf can be used directly
(printf "Hello, %s!\n" "world")
``` 