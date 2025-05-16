//! Alias folding pass for LLLisp
//! 
//! This pass replaces function alias references with their corresponding module function calls.
//! It processes the AST after parsing but before type inference.

use std::collections::HashMap;
use crate::ast::{Program, TopLevel, TopLevelKind, Expr, ExprKind, Located};

/// Represents the alias folding pass
pub struct AliasFolding {
    /// Map of alias names to (module, function) pairs
    aliases: HashMap<String, (String, String)>,
}

impl AliasFolding {
    /// Create a new alias folding pass instance
    pub fn new() -> Self {
        Self {
            aliases: HashMap::new(),
        }
    }
    
    /// Register an alias
    fn register_alias(&mut self, name: &str, module_path: &str, function: &str) {
        // Check if the function part contains slashes, indicating a nested module path
        if function.contains('/') {
            // Split the function part at the last slash
            if let Some(last_slash_pos) = function.rfind('/') {
                let nested_module = format!("{}/{}", module_path, &function[0..last_slash_pos]);
                let actual_function = &function[last_slash_pos + 1..];
                
                self.aliases.insert(name.to_string(), (nested_module, actual_function.to_string()));
            } else {
                // This should never happen if function contains '/'
                self.aliases.insert(name.to_string(), (module_path.to_string(), function.to_string()));
            }
        } else {
            // Standard case - no nesting in the function part
            self.aliases.insert(name.to_string(), (module_path.to_string(), function.to_string()));
        }
    }
    
    /// Process a program and fold aliases
    pub fn process_program(&mut self, program: &Program) -> Program {
        let mut processed_forms = Vec::new();
        
        // First pass: collect all aliases
        for form in &program.forms {
            if let TopLevelKind::Alias { name, module, function } = &form.node {
                self.register_alias(name, module, function);
            }
        }
        
        // Second pass: process all forms and fold aliases
        for form in &program.forms {
            match &form.node {
                // Skip alias declarations in the second pass
                TopLevelKind::Alias { .. } => {},
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
                // Type definitions don't contain expressions, so we can return them as is
                form.clone()
            },
            TopLevelKind::VarDef { name, value } => {
                // Process the value expression
                let processed_value = self.process_expr(value);
                
                // Return the variable definition with the processed expression
                Located::new(
                    TopLevelKind::VarDef {
                        name: name.clone(),
                        value: processed_value,
                    },
                    form.span,
                )
            },
            TopLevelKind::ModuleImport { name: _, path: _, is_header: _ } => {
                // Module imports don't contain expressions, so we can return them as is
                form.clone()
            },
            TopLevelKind::Expr(expr) => {
                // Process the expression
                let processed_expr = self.process_expr_kind(expr);
                
                // Return the expression
                Located::new(
                    TopLevelKind::Expr(processed_expr),
                    form.span,
                )
            },
            TopLevelKind::MacroDef { name, params, body } => {
                // Process the macro body
                let processed_body = self.process_expr(body);
                
                // Return the macro definition with the processed body
                Located::new(
                    TopLevelKind::MacroDef {
                        name: name.clone(),
                        params: params.clone(),
                        body: processed_body,
                    },
                    form.span,
                )
            },
            // Aliases are handled in the first pass
            TopLevelKind::Alias { .. } => {
                panic!("Aliases should be handled in the first pass");
            },
        }
    }
    
    /// Process an expression
    fn process_expr(&self, expr: &Expr) -> Expr {
        let processed_node = self.process_expr_kind(&expr.node);
        Located::new(processed_node, expr.span)
    }
    
    /// Process an expression kind
    fn process_expr_kind(&self, expr_kind: &ExprKind) -> ExprKind {
        match expr_kind {
            ExprKind::Literal(_literal) => {
                // Literals don't need processing
                expr_kind.clone()
            },
            ExprKind::Symbol(name) => {
                // Check if this symbol is an alias
                if let Some((module, function)) = self.aliases.get(name) {
                    // Replace with a module call with no arguments
                    ExprKind::ModuleCall {
                        module: module.clone(),
                        function: function.clone(),
                        args: Vec::new(),
                    }
                } else {
                    // Not an alias, keep as is
                    expr_kind.clone()
                }
            },
            ExprKind::Call { name, args } => {
                // Check if this call is to an alias
                if let Some((module, function)) = self.aliases.get(name) {
                    // Process the arguments
                    let processed_args = args.iter()
                        .map(|arg| self.process_expr(arg))
                        .collect();
                    
                    // Replace with a module call
                    ExprKind::ModuleCall {
                        module: module.clone(),
                        function: function.clone(),
                        args: processed_args,
                    }
                } else {
                    // Not an alias, process arguments normally
                    let processed_args = args.iter()
                        .map(|arg| self.process_expr(arg))
                        .collect();
                    
                    ExprKind::Call {
                        name: name.clone(),
                        args: processed_args,
                    }
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
                
                // Process the body
                let processed_body = self.process_expr(body);
                
                ExprKind::For {
                    iterator: processed_iterator,
                    body: Box::new(processed_body),
                }
            },
            ExprKind::Match { scrutinee, cases } => {
                let processed_scrutinee = self.process_expr(scrutinee);
                
                let processed_cases = cases.iter().map(|case| {
                    crate::ast::MatchCase {
                        pattern: case.pattern.clone(), // Patterns don't contain expressions
                        result: self.process_expr(&case.result),
                    }
                }).collect();
                
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
            ExprKind::Quote(_inner) => {
                // We don't process the body of a quoted expression
                expr_kind.clone()
            },
            ExprKind::Unquote(_inner) => {
                // We don't process unquote expressions
                expr_kind.clone()
            },
            ExprKind::UnquoteSplicing(_inner) => {
                // We don't process unquote-splicing expressions
                expr_kind.clone()
            },
            ExprKind::QuasiQuote(_inner) => {
                // We don't process quasi-quote expressions
                expr_kind.clone()
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
} 