//! Macro expansion pass for LLLisp
//! 
//! This pass expands macro definitions and invocations in the AST.
//! It processes the AST after parsing but before alias folding and type inference.

use std::collections::HashMap;
use crate::ast::{Program, TopLevel, TopLevelKind, Expr, ExprKind, Located, Span, Literal};
use crate::interpreter::Interpreter;

/// Represents a macro definition
#[derive(Clone)]
struct MacroDef {
    /// Parameter names for the macro
    params: Vec<String>,
    /// Macro body (template) expression
    body: Expr,
}

/// Represents the macro expander pass
pub struct MacroExpander {
    /// Map of macro names to their definitions
    macros: HashMap<String, MacroDef>,
    /// Interpreter instance for evaluating macros
    interpreter: Interpreter,
}

impl MacroExpander {
    /// Create a new macro expander pass instance
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            interpreter: Interpreter::new(),
        }
    }
    
    /// Register a macro definition
    fn register_macro(&mut self, name: &str, params: Vec<String>, body: &Expr) {
        // Create a copy of params for interpreter
        let params_clone = params.clone();
        
        self.macros.insert(name.to_string(), MacroDef {
            params: params_clone.clone(),
            body: body.clone(),
        });
        
        // Also register the macro in the interpreter
        // Create a macro in the interpreter's environment
        self.interpreter.define_macro(name, params_clone, body.clone());
    }
    
    /// Process a program and expand macros
    pub fn process_program(&mut self, program: &Program) -> Program {
        let mut processed_forms = Vec::new();
        
        // First pass: collect all macro definitions
        for form in &program.forms {
            if let TopLevelKind::MacroDef { name, params, body } = &form.node {
                self.register_macro(name, params.clone(), body);
            }
        }
        
        // Second pass: process all forms and expand macros
        for form in &program.forms {
            match &form.node {
                // Skip macro definitions in the second pass
                TopLevelKind::MacroDef { .. } => {},
                // Process all other forms
                _ => processed_forms.push(self.process_top_level(form)),
            }
        }
        
        Program { forms: processed_forms }
    }
    
    /// Process a top-level form
    fn process_top_level(&self, form: &TopLevel) -> TopLevel {
        match &form.node {
            TopLevelKind::TypeDef { name: _, ty: _ } => {
                // Type definitions don't contain expressions with macros
                form.clone()
            },
            TopLevelKind::VarDef { name, value } => {
                // Process the value expression to expand macros
                let processed_value = self.process_expr(value);
                
                Located::new(
                    TopLevelKind::VarDef {
                        name: name.clone(),
                        value: processed_value,
                    },
                    form.span,
                )
            },
            TopLevelKind::ModuleImport { name: _, path: _, is_header: _ } => {
                // Module imports don't contain expressions with macros
                form.clone()
            },
            TopLevelKind::Expr(expr) => {
                // Process the expression to expand macros by creating a temporary Expr object
                let temp_expr = Located::new(expr.clone(), form.span);
                let processed_expr = self.process_expr(&temp_expr);
                
                Located::new(
                    TopLevelKind::Expr(processed_expr.node),
                    form.span,
                )
            },
            TopLevelKind::MacroDef { .. } => {
                // Macros are handled in the first pass
                panic!("Macro definitions should be handled in the first pass");
            },
            TopLevelKind::Alias { .. } => {
                // Aliases don't contain expressions with macros
                form.clone()
            },
        }
    }
    
    /// Process an expression
    fn process_expr(&self, expr: &Expr) -> Expr {
        // Process the expression kind
        let new_node = self.process_expr_kind(&expr.node);
        
        // Create a new expression with the same span but processed node
        Expr {
            node: new_node,
            span: expr.span.clone(),
        }
    }
    
    /// Process an expression kind
    fn process_expr_kind(&self, expr_kind: &ExprKind) -> ExprKind {
        match expr_kind {
            ExprKind::Literal(_) => {
                // Literals don't need processing
                expr_kind.clone()
            },
            ExprKind::Symbol(_) => {
                // Symbols don't need expansion
                expr_kind.clone()
            },
            ExprKind::Call { name, args } => {
                // Check if this is a macro call
                if let Some(macro_def) = self.macros.get(name) {
                    println!("Found macro call to '{}'", name);
                    
                    // Process the arguments first (but don't expand macros within them)
                    let processed_args = args.iter()
                        .map(|arg| self.process_expr(arg))
                        .collect::<Vec<_>>();
                    
                    // Expand the macro using the interpreter
                    return self.expand_macro_with_interpreter(name, &processed_args);
                }
                
                // Regular function call - process all arguments
                let processed_args = args.iter()
                    .map(|arg| self.process_expr(arg))
                    .collect();
                
                ExprKind::Call {
                    name: name.clone(),
                    args: processed_args,
                }
            },
            ExprKind::ModuleCall { module, function, args } => {
                // Process arguments in module calls
                let processed_args = args.iter()
                    .map(|arg| self.process_expr(arg))
                    .collect();
                
                ExprKind::ModuleCall {
                    module: module.clone(),
                    function: function.clone(),
                    args: processed_args,
                }
            },
            ExprKind::Addr(inner) => {
                let processed_inner = self.process_expr(inner);
                ExprKind::Addr(Box::new(processed_inner))
            },
            ExprKind::Load(inner) => {
                let processed_inner = self.process_expr(inner);
                ExprKind::Load(Box::new(processed_inner))
            },
            ExprKind::Store { addr, value } => {
                let processed_addr = self.process_expr(addr);
                let processed_value = self.process_expr(value);
                
                ExprKind::Store {
                    addr: Box::new(processed_addr),
                    value: Box::new(processed_value),
                }
            },
            ExprKind::FieldAccess { object, field } => {
                let processed_object = self.process_expr(object);
                
                ExprKind::FieldAccess {
                    object: Box::new(processed_object),
                    field: field.clone(),
                }
            },
            ExprKind::SetField { object, field, value } => {
                let processed_object = self.process_expr(object);
                let processed_value = self.process_expr(value);
                
                ExprKind::SetField {
                    object: Box::new(processed_object),
                    field: field.clone(),
                    value: Box::new(processed_value),
                }
            },
            ExprKind::SetIndex { array, index, value } => {
                let processed_array = self.process_expr(array);
                let processed_index = self.process_expr(index);
                let processed_value = self.process_expr(value);
                
                ExprKind::SetIndex {
                    array: Box::new(processed_array),
                    index: Box::new(processed_index),
                    value: Box::new(processed_value),
                }
            },
            ExprKind::SetAddr { addr, value } => {
                let processed_addr = self.process_expr(addr);
                let processed_value = self.process_expr(value);
                
                ExprKind::SetAddr {
                    addr: Box::new(processed_addr),
                    value: Box::new(processed_value),
                }
            },
            ExprKind::DataConstructor { tag, value } => {
                let processed_value = value.as_ref()
                    .map(|v| Box::new(self.process_expr(v)));
                
                ExprKind::DataConstructor {
                    tag: tag.clone(),
                    value: processed_value,
                }
            },
            ExprKind::Do(expressions) => {
                let processed_expressions = expressions.iter()
                    .map(|expr| self.process_expr(expr))
                    .collect();
                
                ExprKind::Do(processed_expressions)
            },
            ExprKind::If { condition, then_branch, else_branch } => {
                let processed_condition = self.process_expr(condition);
                let processed_then = self.process_expr(then_branch);
                let processed_else = else_branch.as_ref()
                    .map(|e| Box::new(self.process_expr(e)));
                
                ExprKind::If {
                    condition: Box::new(processed_condition),
                    then_branch: Box::new(processed_then),
                    else_branch: processed_else,
                }
            },
            ExprKind::For { iterator, body } => {
                // Process the iterator if present
                let processed_iterator = iterator.as_ref().map(|it| {
                    Box::new(match &**it {
                        crate::ast::ForIterator::Condition(cond) => {
                            crate::ast::ForIterator::Condition(self.process_expr(cond))
                        },
                        crate::ast::ForIterator::Range { var, collection } => {
                            crate::ast::ForIterator::Range {
                                var: var.clone(),
                                collection: self.process_expr(collection),
                            }
                        },
                    })
                });
                
                let processed_body = self.process_expr(body);
                
                ExprKind::For {
                    iterator: processed_iterator,
                    body: Box::new(processed_body),
                }
            },
            ExprKind::Match { scrutinee, cases } => {
                let processed_scrutinee = self.process_expr(scrutinee);
                
                let processed_cases = cases.iter()
                    .map(|case| {
                        crate::ast::MatchCase {
                            pattern: case.pattern.clone(), // Patterns don't contain expressions
                            result: self.process_expr(&case.result),
                        }
                    })
                    .collect();
                
                ExprKind::Match {
                    scrutinee: Box::new(processed_scrutinee),
                    cases: processed_cases,
                }
            },
            ExprKind::TypeCheck { value, check_type } => {
                let processed_value = self.process_expr(value);
                
                ExprKind::TypeCheck {
                    value: Box::new(processed_value),
                    check_type: check_type.clone(),
                }
            },
            ExprKind::Return(value) => {
                let processed_value = self.process_expr(value);
                
                ExprKind::Return(Box::new(processed_value))
            },
            ExprKind::Quote(inner) => {
                // We don't process the inner expression of a quote
                expr_kind.clone()
            },
            ExprKind::Unquote(inner) => {
                // Process the inner expression of an unquote
                let processed_inner = self.process_expr(inner);
                ExprKind::Unquote(Box::new(processed_inner))
            },
            ExprKind::UnquoteSplicing(inner) => {
                // Process the inner expression of an unquote-splicing
                let processed_inner = self.process_expr(inner);
                ExprKind::UnquoteSplicing(Box::new(processed_inner))
            },
            ExprKind::QuasiQuote(inner) => {
                // Process the inner expression of a quasi-quote, handling unquotes
                self.process_quasi_quote(inner)
            },
            ExprKind::Binary { op, left, right } => {
                let processed_left = self.process_expr(left);
                let processed_right = self.process_expr(right);
                
                ExprKind::Binary {
                    op: *op,
                    left: Box::new(processed_left),
                    right: Box::new(processed_right),
                }
            },
        }
    }
    
    /// Process a quasi-quoted expression, handling unquotes
    fn process_quasi_quote(&self, expr: &Expr) -> ExprKind {
        println!("Processing quasi-quote: {:?}", expr.node);
        
        match &expr.node {
            ExprKind::Unquote(inner) => {
                println!("Found unquote in quasi-quote");
                // Process the unquoted expression
                let result = self.process_expr_kind(&inner.node);
                println!("Processed unquote: {:?}", result);
                result
            },
            ExprKind::UnquoteSplicing(inner) => {
                println!("Found unquote-splicing at top level of quasi-quote");
                // Unquote-splicing not valid at top level of quasi-quote
                // Just keep it as is
                ExprKind::QuasiQuote(Box::new(expr.clone()))
            },
            _ => {
                // For other expressions, recursively process their children
                let processed = match &expr.node {
                    ExprKind::Literal(lit) => {
                        println!("Processing literal in quasi-quote: {:?}", lit);
                        expr.node.clone()
                    },
                    ExprKind::Symbol(sym) => {
                        println!("Processing symbol in quasi-quote: {}", sym);
                        expr.node.clone()
                    },
                    ExprKind::Call { name, args } => {
                        println!("Processing call in quasi-quote: {}", name);
                        // Process each argument
                        let processed_args = args.iter()
                            .map(|arg| {
                                if let ExprKind::UnquoteSplicing(inner) = &arg.node {
                                    println!("Found unquote-splicing in call argument");
                                    // Handle splicing by expanding the inner expression
                                    // and expecting it to evaluate to a list
                                    let processed_inner = self.process_expr(inner);
                                    // Splicing is handled during macro expansion
                                    Located::new(
                                        ExprKind::UnquoteSplicing(Box::new(processed_inner)),
                                        arg.span
                                    )
                                } else {
                                    // Regular argument - recursively quasi-quote
                                    let processed_arg = self.process_quasi_quote_expr(arg);
                                    Located::new(
                                        processed_arg,
                                        arg.span
                                    )
                                }
                            })
                            .collect();
                        
                        ExprKind::Call {
                            name: name.clone(),
                            args: processed_args,
                        }
                    },
                    // Handle other expression kinds similarly...
                    _ => {
                        println!("Unhandled expression type in quasi-quote: {:?}", expr.node);
                        // For simplicity, just keep the expression as is
                        // A full implementation would recursively process all children
                        expr.node.clone()
                    }
                };
                
                println!("Keeping processed expression inside quasi-quote");
                // Keep the processed expression inside a quasi-quote
                ExprKind::QuasiQuote(Box::new(Located::new(processed, expr.span)))
            }
        }
    }
    
    /// Process a quasi-quoted expression
    fn process_quasi_quote_expr(&self, expr: &Expr) -> ExprKind {
        match &expr.node {
            ExprKind::Unquote(inner) => {
                // Process the unquoted expression
                self.process_expr_kind(&inner.node)
            },
            _ => {
                // Otherwise keep the expression as is, wrapped in a quasi-quote
                expr.node.clone()
            }
        }
    }
    
    /// Expand a macro using the interpreter
    fn expand_macro_with_interpreter(&self, macro_name: &str, args: &[Expr]) -> ExprKind {
        // Get the macro definition
        if let Some(macro_def) = self.macros.get(macro_name) {
            // Check if the macro has rest parameters
            let has_rest_param = macro_def.params.contains(&"&".to_string());
            
            // Verify argument count if there's no rest parameter
            if !has_rest_param && args.len() != macro_def.params.len() {
                eprintln!("Macro '{}' expects {} arguments, got {}", 
                          macro_name, macro_def.params.len(), args.len());
                return ExprKind::Literal(Literal::Null);
            }
            
            // Convert parameters and arguments to use with macro_substitution
            let call_args = args.iter().cloned().map(|arg| {
                Located::new(
                    arg.node.clone(),
                    arg.span.clone()
                )
            }).collect::<Vec<_>>();
            
            // Apply the macro using the interpreter's substitution method
            // This method doesn't try to evaluate the result, just substitutes arguments
            match self.interpreter.macro_substitution(macro_name, &call_args) {
                Ok(expanded_expr) => {
                    println!("Macro expansion result: {:?}", expanded_expr.node);
                    // Return the expanded expression
                    expanded_expr.node
                }
                Err(e) => {
                    eprintln!("Error expanding macro '{}': {}", macro_name, e);
                    ExprKind::Literal(Literal::Null)
                }
            }
        } else {
            eprintln!("Macro '{}' not found", macro_name);
            ExprKind::Literal(Literal::Null)
        }
    }
    
    /// Convert an interpreter value back to an ExprKind
    fn value_to_expr_kind(&self, value: crate::interpreter::Value) -> ExprKind {
        use crate::interpreter::Value;
        
        match value {
            Value::Integer(n) => ExprKind::Literal(Literal::Integer(n)),
            Value::Float(f) => ExprKind::Literal(Literal::Float(f)),
            Value::Boolean(b) => ExprKind::Literal(Literal::Boolean(b)),
            Value::String(s) => ExprKind::Literal(Literal::String(s)),
            Value::Atom(a) => ExprKind::Literal(Literal::Atom(a)),
            Value::Null => ExprKind::Literal(Literal::Null),
            Value::List(items) => {
                // Convert the list to a tuple literal
                ExprKind::Literal(Literal::Tuple(
                    items.iter().map(|item| {
                        Located::new(
                            self.value_to_expr_kind(item.clone()),
                            Span::new(0, 0)
                        )
                    }).collect()
                ))
            },
            Value::Expression(expr) => expr.node,
            _ => {
                // For other types, return null as a fallback
                ExprKind::Literal(Literal::Null)
            }
        }
    }
} 