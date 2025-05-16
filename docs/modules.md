# LLLisp Module System

LLLisp features a module system that allows code organization and reuse. The module system also provides C interoperability through external function mapping. This document describes how to use modules in LLLisp.

## Module Import

Modules are imported using the `use` keyword. There are two main types of module imports:

1. LLLisp module imports
2. C header imports

### LLLisp Module Imports

To import a LLLisp module:

```lisp
(def module-name (use "path/to/module"))
```

For example:

```lisp
(def math (use "math"))
(def utils (use "utils"))
(def graphics (use "graphics/rendering"))
```

### C Header Imports

To import functions from a C header file:

```lisp
(def module-name (use :header "header_file.h"))
```

For example:

```lisp
(def stdio (use :header "stdio.h"))
(def stdlib (use :header "stdlib.h"))
(def pthread (use :header "pthread.h"))
```

## Using Module Functions

After importing a module, its functions can be accessed using the module name as a prefix:

```lisp
;; Call a function from the math module
(math/sqrt 16.0)

;; Call a function from the C stdio module
(stdio/printf "Hello, %s!\n" "world")
```

### Nested Paths

Modules can have nested paths, separating components with a forward slash:

```lisp
(def graphics (use "graphics"))
(graphics/rendering/draw-triangle x1 y1 x2 y2 x3 y3)
(graphics/ui/button/render "Click Me" 100 200 150 50)
```

## Function Aliases

For frequently used module functions, you can create aliases:

```lisp
;; Import modules
(def stdio (use :header "stdio.h"))
(def math (use "math"))

;; Create aliases for commonly used functions
(alias printf stdio/printf)
(alias sqrt math/sqrt)
(alias sin math/sin)
(alias cos math/cos)

;; Use the aliases directly
(printf "Square root of 16 is %f\n" (sqrt 16.0))
(printf "sin(30°) = %f, cos(60°) = %f\n" (sin 0.5236) (cos 1.0472))
```

## Creating Modules

A LLLisp file automatically serves as a module. Any top-level definitions in a file can be imported by another file.

### Module Example

`math.lllisp`:
```lisp
;; Square function
(def square (fn [(:x f64)] f64
  (* x x)
))

;; Cube function
(def cube (fn [(:x f64)] f64
  (* x x x)
))

;; Calculate distance between two points
(def distance (fn [(:x1 f64) (:y1 f64) (:x2 f64) (:y2 f64)] f64
  (do
    (def dx (- x2 x1))
    (def dy (- y2 y1))
    (sqrt (+ (square dx) (square dy)))
  )
))
```

`main.lllisp`:
```lisp
;; Import the math module
(def math (use "math"))
(def stdio (use :header "stdio.h"))

;; Use functions from the math module
(def result (math/square 5.0))
(stdio/printf "5.0 squared is %f\n" result)

(def dist (math/distance 0.0 0.0 3.0 4.0))
(stdio/printf "Distance from (0,0) to (3,4) is %f\n" dist)
```

## C Interoperability

LLLisp's module system provides seamless interoperability with C libraries:

```lisp
;; Import C standard libraries
(def stdio (use :header "stdio.h"))
(def stdlib (use :header "stdlib.h"))
(def string (use :header "string.h"))

;; Use C functions directly
(def ptr (stdlib/malloc 100))
(string/strcpy ptr "Hello from C!")
(stdio/printf "%s\n" ptr)
(stdlib/free ptr)
```

### External Function Mapping

When a C header is imported, LLLisp uses libclang to parse the header file and extract function signatures. This enables proper type checking when calling C functions.

## Module Namespaces

Modules create their own namespaces. Names defined in a module do not conflict with names in other modules or in the importing file.

```lisp
;; Each module has its own 'init' function that doesn't conflict with others
(def module1 (use "module1"))
(def module2 (use "module2"))

(module1/init)  ;; Calls init from module1
(module2/init)  ;; Calls init from module2

;; Define a local init function that doesn't conflict with module functions
(def init (fn [] void
  (stdio/printf "Initializing main program\n")
))

(init)  ;; Calls the local init
```

## Module Loading and Resolution

The LLLisp compiler resolves module paths relative to the current file and a set of standard library directories. The exact search path depends on the compiler configuration.

For header files, the compiler uses the system's standard include paths and any additional paths specified in the compiler options.

## Example: Building a Modular Application

```lisp
;; config.lllisp
(def app-name "MyApp")
(def version "1.0.0")
(def max-connections 100)

;; logger.lllisp
(def stdio (use :header "stdio.h"))

(def log-info (fn [(:msg str)] void
  (stdio/printf "[INFO] %s\n" msg)
))

(def log-error (fn [(:msg str)] void
  (stdio/printf "[ERROR] %s\n" msg)
))

;; database.lllisp
(def config (use "config"))
(def logger (use "logger"))

(def connect (fn [] bool
  (do
    (logger/log-info "Connecting to database...")
    (logger/log-info (str-concat "Max connections: " (int-to-str config/max-connections)))
    true
  )
))

;; app.lllisp
(def config (use "config"))
(def logger (use "logger"))
(def db (use "database"))
(def stdio (use :header "stdio.h"))

(def main (fn [] i32
  (do
    (stdio/printf "Starting %s v%s\n" config/app-name config/version)
    (logger/log-info "Application starting")
    
    (def db-connected (db/connect))
    (if db-connected
      (logger/log-info "Database connected successfully")
      (logger/log-error "Failed to connect to database")
    )
    
    0  ; Return success
  )
))
``` 