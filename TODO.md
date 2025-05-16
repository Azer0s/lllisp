# LLVM IR Generation TODO

This document outlines the roadmap for implementing LLVM IR generation in the LLLisp compiler.

## Setup and Infrastructure

- [ ] Add LLVM bindings for Rust (inkwell or llvm-sys)
- [ ] Create basic LLVM context management utilities
- [ ] Set up module and function structure
- [ ] Implement basic type conversion from LLLisp types to LLVM types

## Basic Expression Generation

- [ ] Implement IR generation for literals (integers, floats, booleans)
- [ ] Implement IR generation for variables and variable assignments
- [ ] Implement basic arithmetic operations (+, -, *, /, etc.)
- [ ] Implement comparison operations (==, !=, <, >, etc.)

## Control Flow

- [ ] Implement IR generation for if/else expressions
- [ ] Implement IR generation for loops
- [ ] Implement early returns and control flow handling

## Functions and Calling Conventions

- [ ] Implement function declaration and definition
- [ ] Handle function parameters and return types
- [ ] Implement function calls with proper calling conventions
- [ ] Support for external function declarations and FFI

## Advanced Features

- [ ] Implement struct types and struct operations
- [ ] Handle arrays and tuples
- [ ] Implement macro expansion at compile-time
- [ ] Support for generics/polymorphism if applicable

## Memory Management

- [ ] Implement memory allocation and deallocation
- [ ] Handle reference management
- [ ] Implement garbage collection if needed

## Optimization Passes

- [ ] Integrate with LLVM optimization passes
- [ ] Implement any language-specific optimizations
- [ ] Set up different optimization levels

## Testing and Validation

- [ ] Create test suite for LLVM IR generation
- [ ] Implement end-to-end tests comparing interpreter results with compiled code
- [ ] Benchmark performance against the interpreter

## Toolchain Integration

- [ ] Set up compilation pipeline (LLLisp → IR → Object file → Executable)
- [ ] Create command-line options for IR generation and compilation
- [ ] Support debugging information generation

## Documentation

- [ ] Document IR generation architecture
- [ ] Create examples of generated IR for various language constructs
- [ ] Update user documentation with compilation instructions

## Future Enhancements

- [ ] Consider JIT compilation support
- [ ] Explore cross-compilation capabilities
- [ ] Investigate incremental compilation possibilities 