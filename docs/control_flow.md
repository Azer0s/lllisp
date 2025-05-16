# LLLisp Control Flow

LLLisp provides several control flow constructs for conditional execution, iteration, and pattern matching. This document describes the available control flow mechanisms in LLLisp.

## Conditionals

### If Expressions

The basic conditional construct in LLLisp is the `if` expression, which evaluates a condition and executes one of two branches:

```lisp
(if condition
  then-expression
  else-expression)
```

The `if` expression returns the value of the executed branch:

```lisp
(def abs (fn [(:x i32)] i32
  (if (< x 0)
    (- 0 x)  ; then branch
    x        ; else branch
  )
))

(def max (fn [(:a i32) (:b i32)] i32
  (if (> a b) a b)
))
```

### When/Unless Expressions

The `when` and `unless` expressions are convenient shorthands for common `if` patterns:

```lisp
;; when - execute body only if condition is true
(when condition
  expression1
  expression2
  ...)

;; unless - execute body only if condition is false
(unless condition
  expression1
  expression2
  ...)
```

These are typically implemented as macros:

```lisp
(def when (macro [condition & body]
  `(if ,condition
     (do ,@body)
     nil)
))

(def unless (macro [condition & body]
  `(if (not ,condition)
     (do ,@body)
     nil)
))
```

## Loops

### For Loops

The `for` loop is used for iteration. It supports two forms: range-based and condition-based:

#### Range-based For Loop

```lisp
(for (range variable list)
  body)
```

Example:

```lisp
;; Print numbers from 0 to 9
(for (range i (to 10))
  (stdio/printf "%d\n" i)
)

;; Sum an array
(def sum-array (fn [(:arr (ptr i32)) (:len i32)] i32
  (do
    (def result 0)
    (for (range i len)
      (def result (+ result ($ [i] arr)))
    )
    result
  )
))
```

#### Condition-based For Loop (While Loop)

```lisp
(for condition
  body)
```

Example:

```lisp
;; Read until EOF
(def read-all (fn [] str
  (do
    (def result "")
    (def c (stdio/getchar))
    (for (!= c -1)
      (do
        (def result (str-append result (char-to-str c)))
        (def c (stdio/getchar))
      )
    )
    result
  )
))
```

## Pattern Matching

LLLisp's `match` expression provides pattern matching against data structures:

```lisp
(match value
  pattern1 result1
  pattern2 result2
  ...
  _ default-result)  ; _ is a wildcard pattern
```

### Matching Examples

#### Matching on Data Types

```lisp
;; Define an Option type
(type option (data
  [:some i32]
  [:none]
))

;; Match on Option value
(def unwrap-or (fn [(:opt (option i32)) (:default i32)] i32
  (match opt
    [:some val] val
    [:none] default
  )
))
```

#### Matching on Tuples

```lisp
;; Match on tuples (coordinates)
(def describe-point (fn [(:point [i32 i32])] str
  (match point
    [0 0] "origin"
    [0 _] "on y-axis"
    [_ 0] "on x-axis"
    [x y] (if (= x y)
             "on diagonal"
             "elsewhere")
  )
))
```

#### Nested Pattern Matching

```lisp
;; Binary tree
(type tree (data
  [:leaf i32]
  [:node (tuple tree i32 tree)]
))

;; Sum all values in the tree
(def tree-sum (fn [(:t tree)] i32
  (match t
    [:leaf val] val
    [:node [left val right]] (+ (+ (tree-sum left) val) (tree-sum right))
  )
))
```

## Advanced Control Flow

### Multiple Value Returns with Tuples

LLLisp uses tuples for returning multiple values:

```lisp
;; Return both quotient and remainder
(def divmod (fn [(:a i32) (:b i32)] [i32 i32]
  [(/ a b) (% a b)]
))

;; Using the result
(def result (divmod 17 5))
(def quotient ($ [0] result))   ; 3
(def remainder ($ [1] result))  ; 2
```

### Result Types for Error Handling

Using `data` types for error handling (similar to Rust's Result):

```lisp
(type result (data
  [:ok i32]
  [:err str]
))

(def safe-divide (fn [(:a i32) (:b i32)] (result i32)
  (if (= b 0)
    [:err "Division by zero"]
    [:ok (/ a b)]
  )
))

(def process-result (fn [(:r (result i32))] i32
  (match r
    [:ok val] val
    [:err msg] (do
                 (stdio/printf "Error: %s\n" msg)
                 0  ; Default value on error
               )
  )
))
```
