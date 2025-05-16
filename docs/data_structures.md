# LLLisp Data Structures

LLLisp provides several data structures for organizing and managing data. This document describes the available data structures and how to use them.

## Structs

Structs define record types with named fields:

```lisp
(type struct-name (struct
  (:field1 type1)
  (:field2 type2)
  ...
))
```

### Struct Example

```lisp
;; Define a point struct
(type point (struct
  (:x f64)
  (:y f64)
))

;; Define a rectangle struct
(type rectangle (struct
  (:top-left point)
  (:width f64)
  (:height f64)
))

;; Create a point instance
(def p1 (point 10.0 20.0))

;; Create a rectangle instance
(def rect (rectangle (point 0.0 0.0) 100.0 50.0))
```

### Accessing and Modifying Struct Fields

Field access and modification use the `$` operator with a field name preceded by a colon:

```lisp
;; Access struct fields
(def x-coord ($ :x p1))           ; x-coord = 10.0
(def rect-width ($ :width rect))  ; rect-width = 100.0

;; Access nested struct fields
(def top-left-x ($ :x ($ :top-left rect)))  ; top-left-x = 0.0

;; Modify struct fields
($ :x p1 15.0)  ; Set p1.x to 15.0
($ :y p1 25.0)  ; Set p1.y to 25.0

;; Modify nested struct fields
($ :x ($ :top-left rect) 5.0)  ; Set rect.top-left.x to 5.0
```

## Tuples

Tuples are fixed-size collections of heterogeneous values:

```lisp
(type tuple-name (tuple type1 type2 ...))
```

### Tuple Examples

```lisp
;; Define tuple types
(type point-2d (tuple f64 f64))
(type rgb (tuple i32 i32 i32))
(type http-response (tuple i32 str str))  ; Status, content-type, body

;; Create tuple values
(def p (point-2d 1.0 2.0))
(def color (rgb 255 128 0))
(def response (http-response 200 "text/html" "<html>...</html>"))
```

### Shorthand Tuple Notation

A shorthand notation using square brackets is available for both type definitions and values:

```lisp
;; Define tuple types with shorthand notation
(type point-2d [f64 f64])
(type rgb [i32 i32 i32])

;; Create tuple values with shorthand notation
(def p [1.0 2.0])
(def color [255 128 0])
```

### Accessing Tuple Elements

Tuple elements are accessed using the `$` operator (or `nth`) with an index in square brackets:

```lisp
;; Access tuple elements
(def x ($ [0] p))            ; First element: 1.0
(def y ($ [1] p))            ; Second element: 2.0
(def red ($ [0] color))      ; First element: 255
(def status ($ [0] response)) ; First element: 200

(def x (nth 0 p))            ; First element: 1.0
(def y (nth 1 p))            ; Second element: 2.0
(def red (nth 0 color))      ; First element: 255
(def status (nth 0 response)) ; First element: 200
```

## Arrays

Arrays are collections of elements of the same type:

```lisp
;; Dynamic array type (size determined at runtime)
(type float-array [f32])

;; Fixed-size array type (size determined at compile time)
(type int-array-10 [i32, 10])
```

### Array Examples

```lisp
;; Create arrays
(def numbers [i32, 5])             ; Fixed-size array of 5 integers
(def matrix [f64, 3, 3])           ; 3x3 matrix of doubles
(def buffer [u8, 1024])            ; Buffer of 1024 bytes
(def dynamic-array [f64])          ; Dynamic array of doubles

;; Initialize array elements
($ [0] numbers 1)
($ [1] numbers 2)
($ [2] numbers 3)
($ [3] numbers 4)
($ [4] numbers 5)

;; Multidimensional array access (for a 3x3 matrix)
($ [0, 0] matrix 1.0)  ; Row 0, Col 0
($ [1, 2] matrix 5.0)  ; Row 1, Col 2
```

### Array Operations

```lisp
;; Get array length
(def len (array-length numbers))  ; len = 5

;; Array manipulation functions
(array-copy source dest)
(array-fill numbers 0)  ; Fill with zeros
(array-slice numbers 1 3)  ; Extract [1, 2, 3]
```

## Data Types (Tagged Unions)

LLLisp's `data` types define tagged unions, similar to enums or algebraic data types in other languages:

```lisp
(type data-name (data
  [:tag1 type1?]
  [:tag2 type2?]
  ...
))
```

Each variant can optionally have an associated type.

### Data Type Examples

```lisp
;; Option type (similar to Rust's Option or Haskell's Maybe)
(type option (data
  [:some i32]  ; Some variant with an integer value
  [:none]      ; None variant with no value
))

;; Result type (similar to Rust's Result)
(type result (data
  [:ok (ptr void)]  ; Ok variant with a value
  [:err str]        ; Error variant with a string message
))

;; Binary tree
(type tree (data
  [:leaf i32]                   ; Leaf node with value
  [:node (tuple tree i32 tree)] ; Internal node with left, value, right
))
```

### Creating and Using Data Type Values

```lisp
;; Create Option values
(def some-value [:some 42])
(def none-value [:none])

;; Create Result values
(def ok-result [:ok "success"])
(def err-result [:err "failed to process"])

;; Create Tree values
(def leaf1 [:leaf 1])
(def leaf2 [:leaf 3])
(def node [:node [leaf1 2 leaf2]])  ; Node(Leaf(1), 2, Leaf(3))
```

### Pattern Matching

LLLisp supports pattern matching for data types using the `match` expression:

```lisp
;; Define a function that works with Option
(def unwrap-or-default (fn [(:opt (option i32)) (:default i32)] i32
  (match opt
    [:some val] val
    [:none] default
  )
))

;; Apply the function
(unwrap-or-default some-value 0)  ; Returns 42
(unwrap-or-default none-value 0)  ; Returns 0

;; Pattern matching with nested data
(def tree-sum (fn [(:t tree)] i32
  (match t
    [:leaf val] val
    [:node [left val right]] (+ (+ (tree-sum left) val) (tree-sum right))
  )
))
```

## Atoms

Atoms are symbolic values, often used as tags or identifiers:

```lisp
;; Define atom values
(def status-ok :ok)
(def status-error :error)
(def color-red :red)
(def color-green :green)
(def color-blue :blue)

;; Compare atoms
(= status-ok :ok)        ; true
(= color-red color-blue) ; false
```

Atoms are often used with data types:

```lisp
(type http-status atom)

(def ok (http-status :ok))
(def not-found (http-status :not-found))
(def server-error (http-status :server-error))

(type response (tuple http-status str))
(def resp [ok "Request successful"])
```

## Memory Management

LLLisp provides low-level memory operations for working with data structures:

### Address and Dereference Operations

```lisp
;; Get the address of a variable
(def x 42)
(def ptr (addr x))  ; ptr points to x

;; Dereference a pointer
(def value (load ptr))  ; value = 42

;; Store a value at an address
(store ptr 100)  ; x now equals 100
```

### Pointers and Arrays

Arrays automatically decay to pointers in many contexts:

```lisp
(def arr [i32, 5])
(def ptr (addr arr))  ; ptr points to the first element of arr

;; Access elements through pointers
(load ptr)              ; First element
(load (+ ptr 1))        ; Second element
(store (+ ptr 2) 42)    ; Store 42 in the third element
```

### Dynamic Memory Allocation

```lisp
(def stdlib (use :header "stdlib.h"))

;; Allocate memory
(def buffer (stdlib/malloc 1024))  ; Allocate 1024 bytes

;; Use the allocated memory
(store buffer 65)        ; Store 'A' at the beginning
(store (+ buffer 1) 66)  ; Store 'B' at the second byte

;; Free allocated memory
(stdlib/free buffer)
```

## Example: Complex Data Structure

```lisp
;; Define a linked list node
(type list-node (struct
  (:value i32)
  (:next (ptr list-node))
))

;; Create a linked list
(def create-list (fn [] (ptr list-node)
  (do
    (def stdlib (use :header "stdlib.h"))
    
    ;; Create nodes
    (def head (stdlib/malloc (sizeof list-node)))
    (def node2 (stdlib/malloc (sizeof list-node)))
    (def node3 (stdlib/malloc (sizeof list-node)))
    
    ;; Initialize nodes
    ($ :value head 1)
    ($ :next head node2)
    
    ($ :value node2 2)
    ($ :next node2 node3)
    
    ($ :value node3 3)
    ($ :next node3 (ptr list-node null))
    
    head
  )
))

;; Print a linked list
(def print-list (fn [(:head (ptr list-node))] void
  (do
    (def stdio (use :header "stdio.h"))
    (def current head)
    
    (while (!= current null)
      (do
        (stdio/printf "%d -> ", ($ :value current))
        (def current ($ :next current))
      )
    )
    
    (stdio/printf "NULL\n")
  )
)) 