# LLLisp C Interoperability

LLLisp provides seamless interoperability with C libraries through its module system and external function mapping. This document describes how to use C libraries in LLLisp programs.

## Importing C Headers

To import functions from a C header file, use the `use` keyword with the `:header` atom:

```lisp
(def module-name (use :header "header_file.h"))
```

For example:

```lisp
(def stdio (use :header "stdio.h"))
(def stdlib (use :header "stdlib.h"))
(def math (use :header "math.h"))
(def string (use :header "string.h"))
```

## Calling C Functions

After importing a C header, you can call its functions using the module name as a prefix:

```lisp
;; Call printf from stdio
(stdio/printf "Hello, %s!\n" "world")

;; Call malloc and free from stdlib
(def buffer (stdlib/malloc 1024))
(stdlib/free buffer)

;; Call mathematical functions
(def sqrt-result (math/sqrt 16.0))
(def sin-result (math/sin 0.5))
```

## Type Mapping

LLLisp automatically maps C types to corresponding LLLisp types:

| C Type                | LLLisp Type   |
|-----------------------|---------------|
| `char`                | `i8`          |
| `unsigned char`       | `u8`          |
| `short`               | `i16`         |
| `unsigned short`      | `u16`         |
| `int`                 | `i32`         |
| `unsigned int`        | `u32`         |
| `long`                | `i64`         |
| `unsigned long`       | `u64`         |
| `long long`           | `i128`        |
| `unsigned long long`  | `u128`        |
| `float`               | `f32`         |
| `double`              | `f64`         |
| `long double`         | `f128`        |
| `void*`               | `(ptr void)`  |
| `char*`               | `(ptr i8)`    |
| `struct X`            | `(struct ...)`|
| `enum X`              | custom type   |

## Working with C Strings

C strings (null-terminated character arrays) are represented as pointers in LLLisp:

```lisp
;; Create a C string
(def message "Hello, world!")

;; Pass a C string to a C function
(stdio/printf "%s\n" message)

;; Manipulate C strings using string.h functions
(def str1 "Hello")
(def str2 "World")
(def buffer (stdlib/malloc 100))

(string/strcpy buffer str1)
(stdio/printf "Buffer contains: %s\n" buffer)

(string/strcat buffer ", ")
(string/strcat buffer str2)
(stdio/printf "Buffer now contains: %s\n" buffer)

(def length (string/strlen buffer))
(stdio/printf "String length: %d\n" length)

(stdlib/free buffer)
```

## Working with C Structs

LLLisp can define struct types that correspond to C structs:

```lisp
;; Define a struct matching C's 'struct point'
(type point (struct
  (:x i32)
  (:y i32)
))

;; Create and use a point instance
(def p (point 10 20))
(my-c-lib/process-point p)
```

For more complex C structs, make sure the field types and ordering match exactly:

```lisp
;; C struct:
;; struct person {
;;   char name[64];
;;   int age;
;;   float height;
;;   struct address* addr;
;; };

(type address (struct
  (:street (ptr i8))
  (:city (ptr i8))
  (:zip i32)
))

(type person (struct
  (:name [i8, 64])
  (:age i32)
  (:height f32)
  (:addr (ptr address))
))
```

## Function Pointers and Callbacks

LLLisp can pass functions as callbacks to C functions:

```lisp
;; Define a comparison function for qsort
(def compare-ints (fn [(:a (ptr void)) (:b (ptr void))] i32
  (do
    (def int-a (load (ptr i32 a)))
    (def int-b (load (ptr i32 b)))
    (- int-a int-b)  ; Ascending order
  )
))

;; Sort an array of integers using qsort
(def sort-ints (fn [(:arr (ptr i32)) (:len i32)] void
  (stdlib/qsort arr len (sizeof i32) compare-ints)
))

;; Example usage
(def numbers [10 5 8 3 1 7 2 9 6 4])
(sort-ints numbers 10)
```

## Memory Management

When working with C functions that allocate memory, you need to manage that memory explicitly:

```lisp
;; Allocate memory
(def buffer (stdlib/malloc 1024))

;; Check allocation success
(if (= buffer null)
  (do
    (stdio/printf "Memory allocation failed\n")
    (stdlib/exit 1)
  )
)

;; Use the memory
(stdio/sprintf buffer "The answer is %d", 42)
(stdio/printf "%s\n" buffer)

;; Free the memory when done
(stdlib/free buffer)
```

## Error Handling

Many C functions indicate errors through return values or by setting `errno`. LLLisp can check these values:

```lisp
;; Open a file and check for errors
(def file (stdio/fopen "data.txt" "r"))
(if (= file null)
  (do
    (stdio/printf "Error opening file: %s\n" (string/strerror (stdlib/errno)))
    (stdlib/exit 1)
  )
)

;; Use the file
;; ...

;; Close the file when done
(stdio/fclose file)
```

## Variadic Functions

LLLisp can call C variadic functions (functions that take a variable number of arguments) directly:

```lisp
;; Call printf with multiple arguments
(stdio/printf "Int: %d, Float: %f, String: %s\n" 42 3.14 "hello")

;; Define a wrapper for variadic function sprintf
(def format-string (fn [(:format-str (ptr i8)) & args] (ptr i8)
  (do
    (def buffer (stdlib/malloc 1024))
    (stdio/sprintf buffer format-str args)
    buffer
  )
))

;; Use the wrapper
(def result (format-string "Name: %s, Age: %d" "Bob" 42))
(stdio/printf "%s\n" result)
(stdlib/free result)
```

## Common C Libraries Examples

### Standard I/O (stdio.h)

```lisp
(def stdio (use :header "stdio.h"))

;; File operations
(def file (stdio/fopen "data.txt" "w"))
(stdio/fprintf file "Hello, %s!\n" "world")
(stdio/fclose file)

;; Reading input
(def read-line (fn [] (ptr i8)
  (do
    (def buffer (stdlib/malloc 1024))
    (stdio/fgets buffer 1024 stdio/stdin)
    buffer
  )
))

;; Formatted output
(stdio/printf "Int: %d, Float: %.2f, Char: %c\n" 42 3.14159 'A')
```

### Memory Management (stdlib.h)

```lisp
(def stdlib (use :header "stdlib.h"))

;; Dynamic memory allocation
(def buffer (stdlib/malloc 1024))
(stdlib/free buffer)

;; Array allocation
(def array (stdlib/calloc 10 (sizeof i32)))
(stdlib/free array)

;; Resizing memory
(def buffer (stdlib/malloc 100))
(def expanded (stdlib/realloc buffer 200))
(stdlib/free expanded)
```

### Math Functions (math.h)

```lisp
(def math (use :header "math.h"))

;; Basic math functions
(def a (math/sqrt 16.0))    ; Square root: 4.0
(def b (math/pow 2.0 3.0))  ; Power: 8.0
(def c (math/sin 0.0))      ; Sine: 0.0
(def d (math/cos 0.0))      ; Cosine: 1.0
(def e (math/log 2.71828))  ; Natural logarithm: ~1.0
(def f (math/exp 1.0))      ; e^1: ~2.71828
```

### String Manipulation (string.h)

```lisp
(def string (use :header "string.h"))

;; String operations
(def str1 "Hello")
(def str2 "World")

(def cmp (string/strcmp str1 str2))  ; Compare strings
(def len (string/strlen str1))       ; String length: 5
(def pos (string/strchr str1 'e'))   ; Find char in string
(def sub (string/strstr str1 "ll"))  ; Find substring
```

### Time Functions (time.h)

```lisp
(def time (use :header "time.h"))

;; Get current time
(def now (time/time null))
(def time-str (time/ctime (addr now)))
(stdio/printf "Current time: %s\n" time-str)

;; Measure execution time
(def start (time/clock))
;; ... perform some operations ...
(def end (time/clock))
(def elapsed (/ (- end start) (time/CLOCKS_PER_SEC)))
(stdio/printf "Elapsed time: %.2f seconds\n" elapsed)
```

## External Function Mapping

When LLLisp imports a C header file, it uses libclang to parse the header and extract function signatures. This enables proper type checking when calling C functions.

The compiler maintains a mapping between C functions and their LLLisp counterparts, preserving the function signatures, parameter types, and return types.

## Best Practices

1. **Memory management**: Always free memory allocated with C functions
2. **Error checking**: Check return values from C functions for errors
3. **Buffer overflows**: Be careful with string and array operations to avoid buffer overflows
4. **Type compatibility**: Ensure LLLisp types match the expected C types
5. **Thread safety**: Be aware of thread safety issues when using C libraries
6. **Documentation**: Refer to C library documentation for function behavior and requirements 