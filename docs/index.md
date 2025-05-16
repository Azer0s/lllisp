# LLLisp Documentation

LLLisp is a low-level Lisp-like language with a focus on performance and C interoperability. This documentation covers the language's features, syntax, and usage.

## Core Concepts

LLLisp combines the expressiveness of Lisp with the performance of low-level programming. Key features include:

- Lisp-like syntax with S-expressions
- Static typing with type inference
- Direct C interoperability through header imports
- LLVM-based compiler for high performance
- Module system for code organization
- Macro system for metaprogramming
- Memory management control
- Pattern matching for data types

## Documentation Sections

### Language Basics
- [Syntax Overview](syntax_overview.md) - Basic syntax elements and structure
- [Control Flow](control_flow.md) - Conditionals, loops, and branching
- [Types](types.md) - Type system and type definitions
- [Functions](functions.md) - Function definition and usage

### Data Structures
- [Data Structures](data_structures.md) - Structs, tuples, arrays, and tagged unions

### Advanced Features
- [Modules](modules.md) - Module system and organization
- [Macros](macros.md) - Macro system for metaprogramming
- [C Interoperability](c_interop.md) - Working with C libraries and headers
- [Functional Features](functional_features.md) - Higher-order functions and closures

## Quick Examples

### Hello World
```lisp
(def stdio (use :header "stdio.h"))
(stdio/printf "Hello, world!\n")
```

### Basic Function
```lisp
(def square (fn [(:x i32)] i32
  (* x x)
))

(def sum-of-squares (fn [(:a i32) (:b i32)] i32
  (+ (square a) (square b))
))
```

### Data Types
```lisp
(type point (struct
  (:x f64)
  (:y f64)
))

(type shape (data
  [:circle (tuple point f64)]            ; center and radius
  [:rectangle (tuple point point)]       ; top-left and bottom-right
  [:triangle (tuple point point point)]  ; three vertices
))
```

### Pattern Matching
```lisp
(def area (fn [(:s shape)] f64
  (match s
    [:circle [center radius]] (* math/PI (* radius radius))
    [:rectangle [p1 p2]] (* (- ($ :x p2) ($ :x p1)) (- ($ :y p2) ($ :y p1)))
    [:triangle [p1 p2 p3]] (triangle-area p1 p2 p3)
  )
))
```

### C Interoperability
```lisp
(def stdio (use :header "stdio.h"))
(def stdlib (use :header "stdlib.h"))
(def math (use :header "math.h"))

(def main (fn [] i32
  (do
    (stdio/printf "Square root of 16 is %.1f\n" (math/sqrt 16.0))
    (stdio/printf "Value of PI is %.5f\n" math/M_PI)
    
    (def buffer (stdlib/malloc 100))
    (stdio/sprintf buffer "The answer is %d", 42)
    (stdio/puts buffer)
    (stdlib/free buffer)
    
    0  ; Return success
  )
))
```

## Language Design

LLLisp was designed with the following goals in mind:

1. **Performance**: Generate efficient code that can compete with C/C++
2. **Safety**: Provide strong static typing while maintaining flexibility
3. **Expressiveness**: Leverage Lisp's power and simplicity
4. **C Interoperability**: Seamless integration with existing C libraries
5. **Low-level Control**: Direct memory access and control when needed
6. **Functional Programming**: Support for functional programming paradigms

## Getting Started

To get started with LLLisp, see the [Syntax Overview](syntax_overview.md) for basic language concepts. You can then explore more complex features like [Functions](functions.md), [Data Structures](data_structures.md), and [Modules](modules.md).

## Examples

For complete example programs, refer to the `examples/` directory in the LLLisp source repository. 