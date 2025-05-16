// External dependencies
pub extern crate chumsky;
pub extern crate clang;

// Re-export modules
pub mod ast;
pub mod parser;
pub mod type_inference;
pub mod builtins;
pub mod pass1;
pub mod dead_code_elimination;
pub mod external_functions;
pub mod interpreter;
pub mod alias_folding;
pub mod macro_expander;

// Export main functionality
pub use ast::{Expr, ExprKind, Literal, Program, TopLevel, TopLevelKind, Type, Located, Span};
pub use parser::{parse_program, parse_form};
pub use type_inference::{TypeInferer, TypeError};
pub use pass1::CompilerPass1;
pub use dead_code_elimination::DeadCodeElimination;
pub use external_functions::{ExternalFunctionPass, ExternalFunction};
pub use interpreter::{Interpreter, Value, Environment};
pub use alias_folding::AliasFolding;
pub use macro_expander::MacroExpander;
