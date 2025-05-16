use std::collections::HashMap;
use crate::ast::{ExprKind, Literal, Located, Program, TopLevel, TopLevelKind, Span};

/// Represents a runtime value in the interpreter
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i128),
    Float(f64),
    Boolean(bool),
    String(String),
    Char(u8),
    Atom(String),
    Null,
    Tuple(Vec<Value>),
    Function(Function),
    Macro(Macro),
    // For storing unevaluated expressions in macro expansion
    Expression(Box<Located<ExprKind>>),
    List(Vec<Value>),
    NativeFunction(String, usize),
}

impl Value {
    // Convert a Value back to an ExprKind for macro expansion
    fn to_expr(&self) -> Result<Located<ExprKind>, String> {
        match self {
            Value::Integer(n) => Ok(Located::new(
                ExprKind::Literal(Literal::Integer(*n)),
                Span::new(0, 0),
            )),
            Value::Float(f) => Ok(Located::new(
                ExprKind::Literal(Literal::Float(*f)),
                Span::new(0, 0),
            )),
            Value::Boolean(b) => Ok(Located::new(
                ExprKind::Literal(Literal::Boolean(*b)),
                Span::new(0, 0),
            )),
            Value::String(s) => Ok(Located::new(
                ExprKind::Literal(Literal::String(s.clone())),
                Span::new(0, 0),
            )),
            Value::Char(c) => Ok(Located::new(
                ExprKind::Literal(Literal::Char(*c)),
                Span::new(0, 0),
            )),
            Value::Atom(a) => Ok(Located::new(
                ExprKind::Literal(Literal::Atom(a.clone())),
                Span::new(0, 0),
            )),
            Value::Null => Ok(Located::new(
                ExprKind::Literal(Literal::Null),
                Span::new(0, 0),
            )),
            Value::Expression(expr) => Ok(*expr.clone()),
            Value::List(values) => {
                let mut exprs = Vec::new();
                for val in values {
                    exprs.push(val.to_expr()?);
                }
                Ok(Located::new(
                    ExprKind::Literal(Literal::Tuple(exprs)),
                    Span::new(0, 0),
                ))
            },
            _ => Err(format!("Cannot convert value to expression: {:?}", self)),
        }
    }
}

/// Represents a function in the interpreter
#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<String>,
    pub body: Box<Located<ExprKind>>,
    pub env: Environment,
}

impl PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        // Functions are only equal if they're the same object
        // This is a simplification for now
        false
    }
}

/// Represents a macro in the interpreter
#[derive(Debug, Clone)]
pub struct Macro {
    pub params: Vec<String>,
    pub body: Located<ExprKind>,
}

impl PartialEq for Macro {
    fn eq(&self, _other: &Self) -> bool {
        // Macros are only equal if they're the same object
        // This is a simplification for now
        false
    }
}

/// Environment for variable bindings
#[derive(Debug, Clone)]
pub struct Environment {
    bindings: HashMap<String, Value>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: Environment) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.get(name) {
            Some(value.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn set(&mut self, name: &str, value: Value) -> Result<(), String> {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), value);
            Ok(())
        } else if let Some(parent) = &mut self.parent {
            parent.set(name, value)
        } else {
            Err(format!("Undefined variable: {}", name))
        }
    }
}

/// Interpreter for LLLisp
pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = Environment::new();
        Self::add_builtins(&mut env);
        Self {
            env,
        }
    }

    fn add_builtins(env: &mut Environment) {
        // Add OS detection
        env.define("os".to_string(), Value::Atom(std::env::consts::OS.to_string()));
        
        // Add basic arithmetic operators as functions
        env.define("+".to_string(), Value::NativeFunction("+".to_string(), 2));
        env.define("-".to_string(), Value::NativeFunction("-".to_string(), 2));
        env.define("*".to_string(), Value::NativeFunction("*".to_string(), 2));
        env.define("/".to_string(), Value::NativeFunction("/".to_string(), 2));
        env.define("%".to_string(), Value::NativeFunction("%".to_string(), 2));
        
        // Add comparison operators as functions
        env.define("==".to_string(), Value::NativeFunction("==".to_string(), 2));
        env.define("!=".to_string(), Value::NativeFunction("!=".to_string(), 2));
        env.define("<".to_string(), Value::NativeFunction("<".to_string(), 2));
        env.define(">".to_string(), Value::NativeFunction(">".to_string(), 2));
        env.define("<=".to_string(), Value::NativeFunction("<=".to_string(), 2));
        env.define(">=".to_string(), Value::NativeFunction(">=".to_string(), 2));
        
        // Add logical operators as functions
        env.define("and".to_string(), Value::NativeFunction("and".to_string(), 2));
        env.define("or".to_string(), Value::NativeFunction("or".to_string(), 2));
        env.define("not".to_string(), Value::NativeFunction("not".to_string(), 1));
        
        // Add special forms
        env.define("fn".to_string(), Value::NativeFunction("fn".to_string(), usize::MAX)); // Variable args
        env.define("os/when".to_string(), Value::NativeFunction("os/when".to_string(), 2));
    }

    pub fn eval_program(&mut self, program: &Program) -> Result<Value, String> {
        let mut result = Value::Null;

        for form in &program.forms {
            result = self.eval_top_level(form)?;
        }

        Ok(result)
    }

    pub fn eval_top_level(&mut self, form: &TopLevel) -> Result<Value, String> {
        match &form.node {
            TopLevelKind::VarDef { name, value } => {
                let val = self.eval_expr(value)?;
                self.env.define(name.clone(), val.clone());
                Ok(val)
            },
            TopLevelKind::TypeDef { name, ty: _ } => {
                // Register the type name in the environment
                // For the interpreter, we just need to know it's a type
                self.env.define(name.clone(), Value::Expression(Box::new(Located::new(
                    ExprKind::Symbol("type".to_string()),
                    form.span,
                ))));
                Ok(Value::Expression(Box::new(Located::new(
                    ExprKind::Symbol(name.clone()),
                    form.span,
                ))))
            },
            TopLevelKind::ModuleImport { name, path, is_header } => {
                // In the interpreter, we'll just register the module as a special value
                // In a real implementation, we'd load the module's interface
                let module_value = Value::Expression(Box::new(Located::new(
                    ExprKind::Symbol(format!("module:{}:{}", if *is_header { "header" } else { "normal" }, path)),
                    form.span,
                )));
                self.env.define(name.clone(), module_value.clone());
                Ok(module_value)
            },
            TopLevelKind::Expr(expr) => {
                self.eval_expr_kind(expr, &self.env)
            },
            TopLevelKind::MacroDef { name, params, body } => {
                println!("Defining macro: {}", name);
                
                let macro_def = Macro {
                    params: params.clone(),
                    body: body.clone(),
                };
                
                // Store the macro definition
                self.env.define(name.clone(), Value::Macro(macro_def));
                
                Ok(Value::Null)
            },
            TopLevelKind::Alias { name, module, function } => {
                // In the interpreter, we'll register the alias as a special value
                let alias_value = Value::Expression(Box::new(Located::new(
                    ExprKind::Symbol(format!("alias:{}:{}", module, function)),
                    form.span,
                )));
                self.env.define(name.clone(), alias_value.clone());
                Ok(alias_value)
            },
            TopLevelKind::Export { symbols, export_all } => {
                // For interpreter, exports are just metadata - there's no actual effect
                // Print what's being exported to show it's being processed
                if *export_all {
                    println!("Exporting all symbols");
                } else {
                    println!("Exporting symbols: {:?}", symbols);
                }
                Ok(Value::Null)
            },
        }
    }

    pub fn eval_expr(&mut self, expr: &Located<ExprKind>) -> Result<Value, String> {
        self.eval_expr_kind(&expr.node, &self.env)
    }

    fn eval_expr_kind(&self, expr: &ExprKind, env: &Environment) -> Result<Value, String> {
        match expr {
            ExprKind::Literal(lit) => self.eval_literal(lit),
            ExprKind::Symbol(name) => {
                env.get(name).ok_or_else(|| format!("Undefined symbol: {}", name))
            }
            ExprKind::Call { name, args } => {
                // Check if this is a macro call
                if let Some(Value::Macro(macro_def)) = env.get(name) {
                    self.eval_macro_call(&macro_def, args, env)
                } else {
                    self.eval_function_call(name, args, env)
                }
            }
            ExprKind::If { condition, then_branch, else_branch } => {
                let cond_val = self.eval_expr_kind(&condition.node, env)?;
                match cond_val {
                    Value::Boolean(true) => self.eval_expr_kind(&then_branch.node, env),
                    Value::Boolean(false) => {
                        if let Some(else_expr) = else_branch {
                            self.eval_expr_kind(&else_expr.node, env)
                        } else {
                            Ok(Value::Null)
                        }
                    }
                    _ => Err(format!("Condition must be a boolean, got: {:?}", cond_val)),
                }
            }
            ExprKind::Do(exprs) => {
                let mut result = Value::Null;
                for expr in exprs {
                    result = self.eval_expr_kind(&expr.node, env)?;
                }
                Ok(result)
            }
            ExprKind::Quote(expr) => {
                // Return the expression unevaluated
                Ok(Value::Expression(expr.clone()))
            }
            ExprKind::QuasiQuote(expr) => {
                // Evaluate the quasi-quoted expression, expanding unquotes
                self.eval_quasi_quote(&expr.node, env)
            }
            ExprKind::Unquote(_expr) => {
                // Unquote should only be used within a quasi-quote
                Err("Unquote (~) used outside of quasi-quote (`)".to_string())
            },
            ExprKind::UnquoteSplicing(_expr) => {
                // Unquote-splicing should only be used within a quasi-quote
                Err("Unquote-splicing (~@) used outside of quasi-quote (`)".to_string())
            },
            _ => Err(format!("Unsupported expression: {:?}", expr)),
        }
    }

    fn eval_literal(&self, lit: &Literal) -> Result<Value, String> {
        match lit {
            Literal::Integer(n) => Ok(Value::Integer(*n)),
            Literal::Float(f) => Ok(Value::Float(*f)),
            Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Char(c) => Ok(Value::Char(*c)),
            Literal::Atom(a) => Ok(Value::Atom(a.clone())),
            Literal::Null => Ok(Value::Null),
            Literal::Tuple(exprs) => {
                let mut values = Vec::new();
                for expr in exprs {
                    values.push(self.eval_expr_kind(&expr.node, &self.env)?);
                }
                Ok(Value::Tuple(values))
            },
            Literal::List(exprs) => {
                // Lists are handled the same as tuples in the interpreter
                let mut values = Vec::new();
                for expr in exprs {
                    values.push(self.eval_expr_kind(&expr.node, &self.env)?);
                }
                Ok(Value::List(values))
            }
        }
    }

    fn eval_function_call(&self, name: &str, args: &[Located<ExprKind>], env: &Environment) -> Result<Value, String> {
        // Get the function
        let func = env.get(name).ok_or_else(|| format!("Undefined function: {}", name))?;
        
        // Evaluate the arguments
        let mut evaluated_args = Vec::new();
        for arg in args {
            let arg_value = self.eval_expr_kind(&arg.node, env)?;
            evaluated_args.push(arg_value);
        }
        
        // Call the function
        match func {
            Value::Function(func) => {
                if func.params.len() != args.len() {
                    return Err(format!("Expected {} arguments, got {}", func.params.len(), args.len()));
                }
                
                // Create a new environment for the function call
                let mut call_env = Environment::with_parent(func.env.clone());
                
                // Bind the arguments to parameters
                for (param, arg) in func.params.iter().zip(evaluated_args.iter()) {
                    call_env.define(param.clone(), arg.clone());
                }
                
                // Evaluate the body in the new environment
                self.eval_expr_kind(&func.body.node, &call_env)
            },
            Value::NativeFunction(name, expected_args) => {
                // For native functions like +, -, *, /, etc.
                if expected_args != usize::MAX && evaluated_args.len() != expected_args {
                    return Err(format!("Expected {} arguments for native function {}, got {}", 
                                      expected_args, name, evaluated_args.len()));
                }
                
                // Call the native function implementation
                self.eval_native_function(&name, &evaluated_args)
            },
            _ => Err(format!("{} is not a function", name)),
        }
    }

    fn eval_fn_form(&self, args: &[Located<ExprKind>], env: &Environment) -> Result<Value, String> {
        if args.len() < 3 {
            return Err("Function definition requires parameters, return type, and body".to_string());
        }

        // Extract parameter names
        let params = if let ExprKind::Literal(Literal::Tuple(param_exprs)) = &args[0].node {
            let mut param_names = Vec::new();
            for param_expr in param_exprs {
                if let ExprKind::Call { name: param_name, .. } = &param_expr.node {
                    param_names.push(param_name.clone());
                } else {
                    return Err("Invalid parameter in function definition".to_string());
                }
            }
            param_names
        } else {
            return Err("Function parameters must be a tuple".to_string());
        };

        // Return type is at args[1] but we don't use it in the interpreter
        let body = &args[2];

        Ok(Value::Function(Function {
            params,
            body: Box::new(body.clone()),
            env: env.clone(),
        }))
    }

    fn eval_os_when(&self, args: &[Located<ExprKind>], env: &Environment) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("os/when requires two arguments: OS name and body".to_string());
        }

        // Get the OS name
        let os_name = match self.eval_expr_kind(&args[0].node, env)? {
            Value::String(s) => s,
            _ => return Err("First argument to os/when must be a string".to_string()),
        };

        // Check if it matches the current OS
        if os_name == std::env::consts::OS {
            // Evaluate the body
            self.eval_expr_kind(&args[1].node, env)
        } else {
            // Skip the body
            Ok(Value::Null)
        }
    }

    fn eval_macro_call(&self, macro_def: &Macro, args: &[Located<ExprKind>], env: &Environment) -> Result<Value, String> {
        if args.len() != macro_def.params.len() {
            return Err(format!(
                "Macro expected {} arguments, got {}",
                macro_def.params.len(),
                args.len()
            ));
        }

        // Create a new environment for macro expansion
        let mut macro_env = Environment::new();

        // Bind arguments to parameters WITHOUT evaluating them
        for (param, arg) in macro_def.params.iter().zip(args.iter()) {
            // Store the unevaluated expression
            macro_env.define(param.clone(), Value::Expression(Box::new(arg.clone())));
        }

        // Evaluate the macro body to get the expanded form
        let expanded = self.eval_expr_kind(&macro_def.body.node, &macro_env)?;
        
        // Convert the expanded form back to an expression
        let expanded_expr = expanded.to_expr()?;
        
        // Then evaluate the expanded form in the original environment
        self.eval_expr_kind(&expanded_expr.node, env)
    }

    fn eval_quasi_quote(&self, expr: &ExprKind, env: &Environment) -> Result<Value, String> {
        match expr {
            ExprKind::Unquote(inner) => {
                // Evaluate the unquoted expression
                self.eval_expr_kind(&inner.node, env)
            },
            ExprKind::UnquoteSplicing(_inner) => {
                // This should only appear within a list/tuple context
                Err("Unquote-splicing (~@) used in non-list context".to_string())
            },
            ExprKind::Literal(Literal::Tuple(exprs)) => {
                // Process each element, handling unquotes and splices
                let mut result = Vec::new();
                for expr in exprs {
                    match &expr.node {
                        ExprKind::UnquoteSplicing(inner) => {
                            // Evaluate the spliced expression and flatten it into the result
                            let spliced = self.eval_expr_kind(&inner.node, env)?;
                            match spliced {
                                Value::Tuple(values) | Value::List(values) => {
                                    result.extend(values);
                                },
                                _ => return Err("Unquote-splicing (~@) must evaluate to a list or tuple".to_string()),
                            }
                        },
                        _ => {
                            // Recursively process the element
                            let value = self.eval_quasi_quote(&expr.node, env)?;
                            result.push(value);
                        }
                    }
                }
                Ok(Value::List(result))
            },
            ExprKind::Call { name, args } => {
                // Process the function name and arguments
                let mut processed_args = Vec::new();
                for arg in args {
                    processed_args.push(self.eval_quasi_quote(&arg.node, env)?);
                }
                
                // Convert back to an expression
                Ok(Value::Expression(Box::new(Located::new(
                    ExprKind::Call {
                        name: name.clone(),
                        args: vec![], // This is a placeholder
                    },
                    Span::new(0, 0),
                ))))
            },
            // For all other expressions, just return them as unevaluated
            _ => Ok(Value::Expression(Box::new(Located::new(expr.clone(), Span::new(0, 0))))),
        }
    }

    fn eval_native_function(&self, name: &str, args: &[Value]) -> Result<Value, String> {
        match name {
            "+" => {
                if args.len() != 2 {
                    return Err(format!("Expected 2 arguments for +, got {}", args.len()));
                }
                
                // Addition operation
                match (&args[0], &args[1]) {
                    (Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(a + b))
                    },
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(Value::Float(a + b))
                    },
                    (Value::Integer(a), Value::Float(b)) => {
                        Ok(Value::Float(*a as f64 + b))
                    },
                    (Value::Float(a), Value::Integer(b)) => {
                        Ok(Value::Float(a + *b as f64))
                    },
                    _ => Err(format!("Unsupported operand types for +: {:?} and {:?}", args[0], args[1])),
                }
            },
            "*" => {
                if args.len() != 2 {
                    return Err(format!("Expected 2 arguments for *, got {}", args.len()));
                }
                
                // Multiplication operation
                match (&args[0], &args[1]) {
                    (Value::Integer(a), Value::Integer(b)) => {
                        Ok(Value::Integer(a * b))
                    },
                    (Value::Float(a), Value::Float(b)) => {
                        Ok(Value::Float(a * b))
                    },
                    (Value::Integer(a), Value::Float(b)) => {
                        Ok(Value::Float(*a as f64 * b))
                    },
                    (Value::Float(a), Value::Integer(b)) => {
                        Ok(Value::Float(a * *b as f64))
                    },
                    _ => Err(format!("Unsupported operand types for *: {:?} and {:?}", args[0], args[1])),
                }
            },
            // Other native functions can be added here
            _ => Err(format!("Unknown native function: {}", name)),
        }
    }

    /// Define a macro in the environment
    pub fn define_macro(&mut self, name: &str, params: Vec<String>, body: Located<ExprKind>) {
        self.env.define(name.to_string(), Value::Macro(Macro {
            params,
            body,
        }));
    }

    /// Apply a macro to arguments without evaluating the result
    pub fn macro_substitution(&self, macro_name: &str, args: &[Located<ExprKind>]) -> Result<Located<ExprKind>, String> {
        // Get the macro definition
        let macro_def = match self.env.get(macro_name) {
            Some(Value::Macro(m)) => m,
            _ => return Err(format!("Macro '{}' not found", macro_name)),
        };

        println!("Applying macro substitution for '{}' with {} parameters to {} arguments", 
                 macro_name, macro_def.params.len(), args.len());

        // Create a new environment for macro expansion
        let mut macro_env = Environment::new();

        // Bind arguments to parameters without evaluating them
        let mut param_index = 0;
        let mut arg_index = 0;

        while param_index < macro_def.params.len() {
            let param_name = &macro_def.params[param_index];
            
            // Check for rest parameter (indicated by '&')
            if param_name == "&" && param_index < macro_def.params.len() - 1 {
                println!("Found rest parameter");
                // Next parameter is the name for the rest arguments
                param_index += 1;
                let rest_param = &macro_def.params[param_index];
                
                // Collect all remaining arguments into a list
                let rest_args: Vec<Value> = args[arg_index..].iter()
                    .map(|arg| Value::Expression(Box::new(arg.clone())))
                    .collect();
                
                println!("Binding {} rest arguments to parameter '{}'", rest_args.len(), rest_param);
                macro_env.define(rest_param.clone(), Value::List(rest_args));
                
                // We've consumed all remaining arguments
                param_index += 1;  // Move to the next parameter after the rest param
                break;             // Exit the loop as we've processed all arguments
            } else if arg_index < args.len() {
                // Regular parameter with a corresponding argument
                println!("Binding argument to parameter '{}'", param_name);
                macro_env.define(param_name.clone(), Value::Expression(Box::new(args[arg_index].clone())));
                param_index += 1;
                arg_index += 1;
            } else {
                // We ran out of arguments
                return Err(format!("Not enough arguments for macro '{}': expected at least {}, got {}",
                                  macro_name, macro_def.params.len(), args.len()));
            }
        }

        // Create a temporary interpreter with the macro environment
        let mut temp_interpreter = Self::new();
        temp_interpreter.env = macro_env;

        // Use macro substitution instead of evaluation to replace symbols
        let substituted = self.substitute_macro_body(&macro_def.body, &temp_interpreter.env)?;
        
        println!("Substituted macro expression: {:?}", substituted.node);
        Ok(substituted)
    }

    /// Substitute symbols in a macro body with their values from the environment
    fn substitute_macro_body(&self, expr: &Located<ExprKind>, env: &Environment) -> Result<Located<ExprKind>, String> {
        match &expr.node {
            ExprKind::Symbol(name) => {
                // If this is a parameter, replace it with the argument
                if let Some(value) = env.get(name) {
                    match value {
                        Value::Expression(boxed_expr) => {
                            // Return the expression directly without evaluating
                            Ok(*boxed_expr.clone())
                        },
                        Value::List(items) => {
                            // Handle rest parameters by converting list to tuple
                            println!("Converting list parameter '{}' to tuple with {} items", name, items.len());
                            
                            // Extract expressions from the list
                            let mut list_exprs = Vec::new();
                            for item in items {
                                match item {
                                    Value::Expression(expr) => {
                                        list_exprs.push(*expr.clone());
                                    },
                                    _ => {
                                        let expr = item.to_expr()?;
                                        list_exprs.push(expr);
                                    }
                                }
                            }
                            
                            // Create a tuple expression with all arguments
                            Ok(Located::new(
                                ExprKind::Literal(Literal::Tuple(list_exprs)),
                                Span::new(0, 0)
                            ))
                        },
                        _ => {
                            // For non-expression values, convert to an expression
                            value.to_expr()
                        }
                    }
                } else {
                    // Not a parameter, keep it as a symbol
                    Ok(expr.clone())
                }
            },
            ExprKind::Call { name, args } => {
                // Recursively substitute in the arguments
                let mut substituted_args = Vec::new();
                for arg in args {
                    let substituted_arg = self.substitute_macro_body(arg, env)?;
                    substituted_args.push(substituted_arg);
                }
                
                // Return a new call expression with substituted arguments
                Ok(Located::new(
                    ExprKind::Call {
                        name: name.clone(),
                        args: substituted_args,
                    },
                    expr.span.clone()
                ))
            },
            ExprKind::Literal(lit) => {
                match lit {
                    Literal::Tuple(elements) => {
                        // Recursively substitute in tuple elements
                        let mut substituted_elements = Vec::new();
                        for elem in elements {
                            let substituted_elem = self.substitute_macro_body(elem, env)?;
                            substituted_elements.push(substituted_elem);
                        }
                        
                        Ok(Located::new(
                            ExprKind::Literal(Literal::Tuple(substituted_elements)),
                            expr.span.clone()
                        ))
                    },
                    _ => Ok(expr.clone()),
                }
            },
            // Other expression types can be added here as needed
            // For now, we'll just return most other expressions unchanged
            _ => Ok(expr.clone()),
        }
    }

    /// Evaluate a function expression with the given arguments
    pub fn eval_function(&self, fn_expr: &Located<ExprKind>, args: &[Located<ExprKind>]) -> Result<Value, String> {
        // Evaluate the function expression to get a function value
        match &fn_expr.node {
            ExprKind::Call { name, args: fn_args } if name == "fn" => {
                // Extract parameter names
                if fn_args.len() < 3 {
                    return Err("Function definition requires parameters, return type, and body".to_string());
                }
                
                let param_names = if let ExprKind::Literal(Literal::Tuple(param_exprs)) = &fn_args[0].node {
                    let mut names = Vec::new();
                    for param_expr in param_exprs {
                        if let ExprKind::Symbol(param_name) = &param_expr.node {
                            names.push(param_name.clone());
                        } else {
                            return Err(format!("Invalid parameter in function definition: {:?}", param_expr.node));
                        }
                    }
                    names
                } else {
                    return Err(format!("Invalid parameters list in function definition: {:?}", fn_args[0].node));
                };
                
                // The body is the third argument (index 2)
                let body = &fn_args[2];
                
                // Check if we have the correct number of args
                if param_names.len() != args.len() {
                    return Err(format!("Expected {} arguments, got {}", param_names.len(), args.len()));
                }
                
                // Create a new environment for the function execution
                let mut call_env = Environment::with_parent(self.env.clone());
                
                // Bind the provided arguments to the parameter names
                for (param, arg) in param_names.iter().zip(args.iter()) {
                    let arg_value = self.eval_expr_kind(&arg.node, &self.env)?;
                    call_env.define(param.clone(), arg_value);
                }
                
                // Evaluate the body in the new environment
                self.eval_expr_kind(&body.node, &call_env)
            },
            _ => Err(format!("Expected function expression, got: {:?}", fn_expr.node)),
        }
    }

    /// Add a native function to the interpreter
    pub fn add_native_function(&mut self, name: &str, arg_count: usize) {
        self.env.define(name.to_string(), Value::NativeFunction(name.to_string(), arg_count));
    }

    /// Apply a macro to arguments and evaluate the result
    pub fn apply_macro(&mut self, macro_name: &str, args: &[Located<ExprKind>]) -> Result<Located<ExprKind>, String> {
        // First do the substitution
        let substituted = self.macro_substitution(macro_name, args)?;
        
        // Then evaluate the result
        let expanded_value = self.eval_expr(&substituted)?;
        
        // Convert the result back to an expression
        match expanded_value {
            Value::Expression(expr) => Ok(*expr),
            Value::List(values) => {
                println!("Converting list to tuple expression");
                // Convert list to a tuple expression
                let exprs: Result<Vec<Located<ExprKind>>, String> = values.iter()
                    .map(|v| v.to_expr())
                    .collect();
                
                let tuple_exprs = exprs?;
                Ok(Located::new(
                    ExprKind::Literal(Literal::Tuple(tuple_exprs)),
                    Span::new(0, 0) // Use a dummy span
                ))
            },
            other => {
                // For other values, convert to expressions
                other.to_expr()
            }
        }
    }

    /// Get a variable value by name
    pub fn get_var(&self, name: &str) -> Option<Value> {
        self.env.get(name).map(|v| v.clone())
    }
} 