use std::collections::HashMap;
use crate::ast::{ExprKind, Literal, Located, Program, TopLevel, TopLevelKind, Type, BinaryOp, Span};

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
    pub body: Box<Located<ExprKind>>,
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
        
        // Add basic arithmetic functions
        // These will be implemented as native functions
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
            }
            TopLevelKind::TypeDef { .. } => {
                // Types are not evaluated at runtime in this simple interpreter
                Ok(Value::Null)
            }
            TopLevelKind::ModuleImport { .. } => {
                // Module imports are not handled in this simple interpreter
                Ok(Value::Null)
            }
            TopLevelKind::Expr(expr) => {
                self.eval_expr_kind(expr, &self.env)
            }
            TopLevelKind::MacroDef { name, params, body } => {
                // Define a macro
                let macro_val = Value::Macro(Macro {
                    params: params.clone(),
                    body: Box::new(body.clone()),
                });
                self.env.define(name.clone(), macro_val.clone());
                Ok(macro_val)
            }
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
            ExprKind::Binary { op, left, right } => {
                let left_val = self.eval_expr_kind(&left.node, env)?;
                let right_val = self.eval_expr_kind(&right.node, env)?;
                self.eval_binary_op(*op, left_val, right_val)
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
        // Handle special forms first
        match name {
            "fn" => self.eval_fn_form(args, env),
            "os/when" => self.eval_os_when(args, env),
            _ => {
                // Regular function call
                // Evaluate all arguments
                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.eval_expr_kind(&arg.node, env)?);
                }

                // Look up the function in the environment
                match env.get(name) {
                    Some(Value::Function(func)) => self.apply_function(&func, &arg_values),
                    _ => Err(format!("Undefined function: {}", name)),
                }
            }
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

    fn apply_function(&self, func: &Function, args: &[Value]) -> Result<Value, String> {
        if args.len() != func.params.len() {
            return Err(format!(
                "Function expected {} arguments, got {}",
                func.params.len(),
                args.len()
            ));
        }

        // Create a new environment with the function's environment as parent
        let mut call_env = Environment::with_parent(func.env.clone());

        // Bind arguments to parameters
        for (param, arg) in func.params.iter().zip(args.iter()) {
            call_env.define(param.clone(), arg.clone());
        }

        // Evaluate the body in the new environment
        self.eval_expr_kind(&func.body.node, &call_env)
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

    fn eval_binary_op(&self, op: BinaryOp, left: Value, right: Value) -> Result<Value, String> {
        match op {
            BinaryOp::Add => match (&left, &right) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                _ => Err(format!("Cannot add {:?} and {:?}", left, right)),
            },
            BinaryOp::Sub => match (&left, &right) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                _ => Err(format!("Cannot subtract {:?} and {:?}", left, right)),
            },
            BinaryOp::Mul => match (&left, &right) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                _ => Err(format!("Cannot multiply {:?} and {:?}", left, right)),
            },
            BinaryOp::Div => match (&left, &right) {
                (Value::Integer(a), Value::Integer(b)) => {
                    if *b == 0 {
                        Err("Division by zero".to_string())
                    } else {
                        Ok(Value::Integer(a / b))
                    }
                },
                (Value::Float(a), Value::Float(b)) => {
                    if *b == 0.0 {
                        Err("Division by zero".to_string())
                    } else {
                        Ok(Value::Float(a / b))
                    }
                },
                _ => Err(format!("Cannot divide {:?} and {:?}", left, right)),
            },
            BinaryOp::Eq => Ok(Value::Boolean(left == right)),
            BinaryOp::Ne => Ok(Value::Boolean(left != right)),
            BinaryOp::Lt => match (&left, &right) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a < b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(format!("Cannot compare {:?} and {:?}", left, right)),
            },
            BinaryOp::Gt => match (&left, &right) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a > b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a > b)),
                _ => Err(format!("Cannot compare {:?} and {:?}", left, right)),
            },
            BinaryOp::Le => match (&left, &right) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a <= b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a <= b)),
                _ => Err(format!("Cannot compare {:?} and {:?}", left, right)),
            },
            BinaryOp::Ge => match (&left, &right) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a >= b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Boolean(a >= b)),
                _ => Err(format!("Cannot compare {:?} and {:?}", left, right)),
            },
            BinaryOp::Mod => match (&left, &right) {
                (Value::Integer(a), Value::Integer(b)) => {
                    if *b == 0 {
                        Err("Modulo by zero".to_string())
                    } else {
                        Ok(Value::Integer(a % b))
                    }
                },
                _ => Err(format!("Cannot perform modulo on {:?} and {:?}", left, right)),
            },
        }
    }
} 