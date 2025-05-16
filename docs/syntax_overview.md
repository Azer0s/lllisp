# LLLisp Syntax Overview

LLLisp is a low-level Lisp-like language with a focus on performance and C interoperability. This document provides an overview of the basic syntax elements.

## File Extensions and Naming Conventions

- LLLisp source files use the `.lllisp` or `.l3` file extension
- Source code follows a consistent naming convention:
  - Identifiers use kebab-case (words separated by hyphens)
  - Module and submodule names are separated by forward slashes (e.g., `graphics/rendering/draw-triangle`)
  - Variable and function names are typically lowercase with hyphens (e.g., `calculate-distance`)
  - Type names are also lowercase with hyphens (e.g., `point-2d`)

## Basic Syntax

LLLisp uses S-expressions with a Lisp-like syntax. All expressions are enclosed in parentheses, with the first element typically being an operator or function name.

```lisp
(operation arg1 arg2 ...)
```

### Comments

Comments in LLLisp start with a semicolon (`;`) and continue to the end of the line:

```lisp
;; This is a comment
(def x 10)  ; This is an inline comment
```

### Variable Definition

Variables are defined using the `def` keyword:

```lisp
(def name value)
```

For example:

```lisp
(def answer 42)
(def pi 3.14159)
(def message "Hello, world!")
```

### Type Annotations

Type annotations can be added when defining variables:

```lisp
(def x (i32 42))     ; 32-bit integer
(def y (f64 3.14))   ; 64-bit float
(def c (char 'A'))   ; Character
```

### Basic Types

LLLisp provides several built-in types:

- Integer types: `i8`, `i16`, `i32`, `i64`, `i128`
- Unsigned integer types: `u8`, `u16`, `u32`, `u64`, `u128`
- Floating-point types: `f32`, `f64`, `f128`
- Boolean type: `bool`
- Character type: `char` (usually aliased to `u8`)
- String literals (implemented as pointer to characters)
- Atom type: `atom` (similar to symbols in other Lisps)

### Basic Operations

Arithmetic operations follow prefix notation:

```lisp
(+ 1 2)          ; Addition: 3
(- 10 5)         ; Subtraction: 5
(* 4 3)          ; Multiplication: 12
(/ 10 2)         ; Division: 5
(% 10 3)         ; Modulo: 1
```

Comparison operations:

```lisp
(< 1 2)          ; Less than: true
(> 5 2)          ; Greater than: true
(<= 5 5)         ; Less than or equal: true
(>= 6 5)         ; Greater than or equal: true
(= 5 5)          ; Equality: true
(!= 5 6)         ; Inequality: true
```

Logical operations:

```lisp
(and true false) ; Logical AND: false
(or true false)  ; Logical OR: true
(not true)       ; Logical NOT: false
```

### Conditionals

The if expression has the form:

```lisp
(if condition true-branch false-branch)
```

For example:

```lisp
(def abs (fn [(:x i32)] i32
  (if (< x 0)
    (- 0 x)  ; true branch
    x        ; false branch
  )
))
```

### Block Expression

The `do` expression allows multiple expressions to be evaluated in sequence:

```lisp
(do
  (def x 10)
  (def y 20)
  (+ x y)  ; The value of the last expression is returned
)
```

## Memory Operations

LLLisp provides low-level memory operations:

- `addr` - get the address of a variable
- `load` - load a value from a pointer
- `store` - store a value to a pointer

```lisp
(def x 42)
(def ptr (addr x))   ; Get address of x
(load ptr)           ; Load value from pointer: 42
(store ptr 100)      ; Store 100 at the address of x
```

## Next Steps

For more detailed information on specific language features, see the following documents:
- [Types](types.md) - Type system and type definitions
- [Functions](functions.md) - Function definitions and higher-order functions
- [Modules](modules.md) - Module system and imports
- [Data Structures](data_structures.md) - Structs, tuples, and data types
- [Macros](macros.md) - Macro system for metaprogramming 