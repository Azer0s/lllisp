# LLLisp

A low-level Lisp-like language with a focus on performance and C interoperability.

## Features

* Lisp-like syntax with static typing
* Direct C interoperability through external function mapping
* LLVM-based compiler for high performance
* Dead code elimination and optimization passes
* Type inference system
* Module system with nested module paths support

## Getting Started

### Prerequisites

* Rust (latest stable)
* LLVM
* Clang/libclang

### Building

```bash
cargo build --release
```

## Usage

### Basic Syntax

```lisp
;; Import a module
(def stdio (use :header "stdio.h"))

;; Variable definition
(def answer 42)

;; Function definition
(def square (fn [(:x i32)] i32
  (* x x)))

;; Type definition
(type Point (struct
  (:x i32)
  (:y i32)
))

;; Conditional expressions
(def abs (fn [(:x i32)] i32
  (if (< x 0)
    (- 0 x)
    x)))

(alias printf stdio/printf)

(printf "Hello, %s!\n" "world")
```

### C Interoperability

LLLisp can import and use C functions directly from header files:

```lisp
;; Import the stdio header
(def stdio (use :header "stdio.h"))

;; Use printf from stdio
(def print-message (fn [] void
  (do
    (stdio/printf "Hello, %s!\n" "world")
    (stdio/printf "The answer is %d\n" 42)
  )
))
```

## Compiler Passes

LLLisp includes several compiler passes for optimization:

1. **Dead Code Elimination** - Removes unreachable code after return statements, eliminates unused variable definitions, and performs constant folding.

2. **Type Inference** - Infers types for all variables and expressions, ensuring type safety and enabling optimizations.

3. **Constant Folding** - Evaluates constant expressions at compile time, reducing runtime overhead.

4. **External Function Mapping** - Uses libclang to parse C header files and extract function signatures for proper C interoperability.

## License

This project is licensed under the MIT License - see the LICENSE file for details. 