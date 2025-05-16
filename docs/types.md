# LLLisp Type System

LLLisp features a static type system with type inference. This document describes the type system and how to work with types in LLLisp.

## Built-in Types

LLLisp provides several built-in primitive types:

### Integer Types

- `i8`: 8-bit signed integer
- `i16`: 16-bit signed integer
- `i32`: 32-bit signed integer
- `i64`: 64-bit signed integer
- `i128`: 128-bit signed integer

### Unsigned Integer Types

- `u8`: 8-bit unsigned integer
- `u16`: 16-bit unsigned integer
- `u32`: 32-bit unsigned integer
- `u64`: 64-bit unsigned integer
- `u128`: 128-bit unsigned integer

### Floating-Point Types

- `f16`: 16-bit floating-point number
- `f32`: 32-bit floating-point number
- `f64`: 64-bit floating-point number
- `f128`: 128-bit floating-point number

### Other Primitive Types

- `bool`: Boolean type (true or false)
- `atom`: Atom type for symbolic constants

## Type Definitions

Types are defined using the `type` keyword followed by the name and the type expression:

```lisp
(type name type-expression)
```

### Type Aliases

Type aliases give a name to an existing type:

```lisp
(type char u8)           ; Alias char to u8
(type str (ptr char))    ; Alias str to pointer to char
(type int i32)           ; Alias int to i32
```

### Pointer Types

Pointer types are created using the `ptr` type constructor:

```lisp
(type int-ptr (ptr i32))     ; Pointer to i32
(type char-ptr (ptr char))   ; Pointer to char
(type void-ptr (ptr void))   ; Void pointer
```

### Array Types

Array types are defined using square brackets:

```lisp
(type float-array [f32])         ; Dynamic array of f32
(type int-array-10 [i32, 10])    ; Fixed-size array of 10 i32 elements
(type matrix [f64, 3, 3])        ; 3x3 matrix of f64
```

### Function Types

Function types specify the parameter types and return type:

```lisp
(type unary-op (fn [i32] i32))                   ; Function taking i32, returning i32
(type binary-op (fn [i32 i32] i32))              ; Function taking two i32, returning i32
(type callback (fn [str i32 bool] void))         ; Callback function
(type comparator (fn [(ptr void) (ptr void)] i32)) ; Comparator function for sorting
```

### Struct Types

Struct types are defined with the `struct` keyword, followed by field definitions:

```lisp
(type point (struct
  (:x f64)
  (:y f64)
))

(type person (struct
  (:name str)
  (:age i32)
  (:is-active bool)
))

(type rectangle (struct
  (:top-left point)
  (:width f64)
  (:height f64)
))
```

### Tuple Types

Tuple types are heterogeneous fixed-size collections:

```lisp
(type pair (tuple i32 i32))                     ; Pair of i32 values
(type triple (tuple f64 f64 f64))               ; Triple of f64 values
(type string-int-pair (tuple str i32))          ; Pair of string and integer
(type http-response (tuple i32 str str))        ; HTTP status, content-type, body
```

A shorthand syntax is also available using square brackets:

```lisp
(type pair [i32 i32])
(type coordinates [f64 f64 f64])
```

### Data Types (Tagged Unions)

Data types represent tagged unions (similar to enums or variants in other languages):

```lisp
(type option (data
  [:some i32]
  [:none]
))

(type result (data
  [:ok (ptr void)]
  [:err str]
))

(type json (data
  [:null]
  [:bool bool]
  [:number f64]
  [:string str]
  [:array (ptr (array json))]
  [:object (ptr (map str json))]
))
```

### Union Types

Union types represent values that can be one of several types:

```lisp
(type number (union i32 f64))
(type any (union i32 f64 bool str))
```

## Generic Types

LLLisp supports generic type definitions with type parameters:

```lisp
(type pair<T1 T2> (struct
  (:first T1)
  (:second T2)
))

(type option<T> (data
  [:some T]
  [:none]
))

(type result<T E> (data
  [:ok T]
  [:err E]
))

(type vec<T> (struct
  (:data (ptr T))
  (:length i64)
  (:capacity i64)
))
```

## Type Checking

Types are checked at compile time. The `is` operator can be used to perform runtime type checks:

```lisp
(def value 42)
(is i32 value)        ; Returns true
(is f64 value)        ; Returns false

(def option-value [:some 42])
(is (option i32) option-value)     ; Returns true
```

## Type Inference

LLLisp features type inference so you often don't need to specify types explicitly:

```lisp
(def x 42)             ; Inferred as i32
(def y 3.14)           ; Inferred as f64
(def z (+ x 10))       ; Inferred as i32
(def s "hello")        ; Inferred as str

(def add (fn [a b]     ; Parameter and return types inferred
  (+ a b)
))
```

However, explicit type annotations are sometimes necessary for clarity or when inference is ambiguous:

```lisp
(def parse-int (fn [(:s str)] i32
  ;; Implementation here
))

(def process-array (fn [(:arr (ptr i32)) (:len i32)] void
  ;; Implementation here
))
``` 