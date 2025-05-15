/// First compiler pass for LLLisp
/// 
/// This pass transforms the AST after type inference to prepare it for LLVM code generation.
/// It performs the following transformations:
/// 1. Monomorphizes generic type definitions by creating concrete instances
/// 2. Resolves all type aliases to concrete types
/// 3. Prepares data structures for LLVM IR generation
/// 4. Resolves generic function calls to monomorphized versions
/// 5. Makes implicit returns explicit by adding Return nodes

use std::collections::{HashMap, HashSet};
use crate::ast::{Program, TopLevel, TopLevelKind, Type, Expr, ExprKind, Located, Span};

/// Represents a monomorphized type with a unique name
#[derive(Debug, Clone)]
pub struct MonomorphizedType {
    /// Original type name
    pub original_name: String,
    /// Unique mangled name for the concrete instantiation
    pub mangled_name: String,
    /// The concrete type with all type parameters substituted
    pub concrete_type: Type,
    /// Type arguments used for this instantiation
    pub type_args: Vec<Type>,
}

/// Represents a function that has been monomorphized from a generic template
#[derive(Debug, Clone)]
pub struct MonomorphizedFunction {
    /// Original function name
    pub original_name: String,
    /// Unique mangled name for this concrete instantiation
    pub mangled_name: String,
    /// Type arguments used for this instantiation
    pub type_args: Vec<Type>,
    /// The monomorphized function definition
    pub definition: TopLevel,
}

/// Compiler pass that prepares the AST for LLVM code generation
pub struct CompilerPass1 {
    /// Map of type names to their definitions
    type_definitions: HashMap<String, Type>,
    /// Map of generic type names to their definitions
    generic_type_definitions: HashMap<String, Type>,
    /// Set of all used concrete type instantiations
    concrete_type_instances: HashSet<String>,
    /// Map of mangled names to monomorphized type instances
    monomorphized_types: HashMap<String, MonomorphizedType>,
    /// Map of mangled names to monomorphized function instances
    monomorphized_functions: HashMap<String, MonomorphizedFunction>,
}

impl CompilerPass1 {
    /// Create a new compiler pass
    pub fn new() -> Self {
        Self {
            type_definitions: HashMap::new(),
            generic_type_definitions: HashMap::new(),
            concrete_type_instances: HashSet::new(),
            monomorphized_types: HashMap::new(),
            monomorphized_functions: HashMap::new(),
        }
    }

    /// Process a program and transform it for LLVM code generation
    pub fn process(&mut self, program: &Program) -> Program {
        // First, collect all type definitions
        self.collect_type_definitions(program);
        
        // Find all generic type instantiations in the program
        self.collect_generic_instances(program);
        
        // Create monomorphized versions of all generic types
        let mut transformed_forms = Vec::new();
        
        // Process all forms, adding explicit returns where needed
        for form in &program.forms {
            match &form.node {
                TopLevelKind::TypeDef { name: _, ty } => {
                    // Skip generic type definitions, we'll add monomorphized versions later
                    if !self.is_generic_type(ty) {
                        transformed_forms.push(form.clone());
                    }
                },
                TopLevelKind::VarDef { name, value } => {
                    // Process the value expression to add explicit returns
                    let processed_value = self.process_expr(value);
                    let new_form = TopLevel {
                        span: form.span,
                        node: TopLevelKind::VarDef {
                            name: name.clone(),
                            value: processed_value,
                        },
                    };
                    transformed_forms.push(new_form);
                },
                _ => transformed_forms.push(form.clone()),
            }
        }
        
        // Add all monomorphized type definitions
        for mono_type in self.monomorphized_types.values() {
            let type_def = TopLevel {
                span: Span::new(0, 0), // We could preserve the original span here
                node: TopLevelKind::TypeDef {
                    name: mono_type.mangled_name.clone(),
                    ty: mono_type.concrete_type.clone(),
                },
            };
            transformed_forms.push(type_def);
        }
        
        // Add all monomorphized function definitions
        for mono_func in self.monomorphized_functions.values() {
            transformed_forms.push(mono_func.definition.clone());
        }
        
        // Return the transformed program
        Program { forms: transformed_forms }
    }
    
    /// Process an expression to add explicit returns where needed
    fn process_expr(&mut self, expr: &Expr) -> Expr {
        let processed_node = match &expr.node {
            ExprKind::Do(exprs) => {
                // Process a block of expressions
                self.process_block(exprs, expr.span)
            },
            ExprKind::If { condition, then_branch, else_branch } => {
                // Process conditional expressions
                let processed_condition = self.process_expr(condition);
                let processed_then = self.process_expr(then_branch);
                
                // Wrap then branch in a Return if it's not already a Return
                let wrapped_then = match &processed_then.node {
                    ExprKind::Return(_) => processed_then,
                    _ => Located::new(
                        ExprKind::Return(Box::new(processed_then)),
                        then_branch.span,
                    ),
                };
                
                let processed_else = if let Some(else_expr) = else_branch {
                    let processed_else_expr = self.process_expr(else_expr);
                    
                    // Wrap else branch in a Return if it's not already a Return
                    let wrapped_else = match &processed_else_expr.node {
                        ExprKind::Return(_) => processed_else_expr,
                        _ => Located::new(
                            ExprKind::Return(Box::new(processed_else_expr)),
                            else_expr.span,
                        ),
                    };
                    
                    Some(Box::new(wrapped_else))
                } else {
                    None
                };
                
                ExprKind::If {
                    condition: Box::new(processed_condition),
                    then_branch: Box::new(wrapped_then),
                    else_branch: processed_else,
                }
            },
            ExprKind::Match { scrutinee, cases } => {
                // Process match expressions
                let processed_scrutinee = self.process_expr(scrutinee);
                let processed_cases = cases.iter()
                    .map(|case| {
                        let processed_result = self.process_expr(&case.result);
                        crate::ast::MatchCase {
                            pattern: case.pattern.clone(),
                            result: processed_result,
                        }
                    })
                    .collect();
                
                ExprKind::Match {
                    scrutinee: Box::new(processed_scrutinee),
                    cases: processed_cases,
                }
            },
            ExprKind::For { iterator, body } => {
                // Process for loop expressions
                let processed_body = self.process_expr(body);
                
                ExprKind::For {
                    iterator: iterator.clone(),
                    body: Box::new(processed_body),
                }
            },
            ExprKind::Binary { op, left, right } => {
                // Process binary operations
                let processed_left = self.process_expr(left);
                let processed_right = self.process_expr(right);
                
                ExprKind::Binary {
                    op: op.clone(),
                    left: Box::new(processed_left),
                    right: Box::new(processed_right),
                }
            },
            ExprKind::Call { name, args } => {
                // Process function calls
                let processed_args = args.iter()
                    .map(|arg| self.process_expr(arg))
                    .collect();
                
                ExprKind::Call {
                    name: name.clone(),
                    args: processed_args,
                }
            },
            ExprKind::ModuleCall { module, function, args } => {
                // Process module function calls
                let processed_args = args.iter()
                    .map(|arg| self.process_expr(arg))
                    .collect();
                
                ExprKind::ModuleCall {
                    module: module.clone(),
                    function: function.clone(),
                    args: processed_args,
                }
            },
            ExprKind::FieldAccess { object, field } => {
                // Process field access
                let processed_object = self.process_expr(object);
                
                ExprKind::FieldAccess {
                    object: Box::new(processed_object),
                    field: field.clone(),
                }
            },
            ExprKind::SetField { object, field, value } => {
                // Process field setting
                let processed_object = self.process_expr(object);
                let processed_value = self.process_expr(value);
                
                ExprKind::SetField {
                    object: Box::new(processed_object),
                    field: field.clone(),
                    value: Box::new(processed_value),
                }
            },
            ExprKind::SetIndex { array, index, value } => {
                // Process index setting
                let processed_array = self.process_expr(array);
                let processed_index = self.process_expr(index);
                let processed_value = self.process_expr(value);
                
                ExprKind::SetIndex {
                    array: Box::new(processed_array),
                    index: Box::new(processed_index),
                    value: Box::new(processed_value),
                }
            },
            ExprKind::Store { addr, value } => {
                // Process store operations
                let processed_addr = self.process_expr(addr);
                let processed_value = self.process_expr(value);
                
                ExprKind::Store {
                    addr: Box::new(processed_addr),
                    value: Box::new(processed_value),
                }
            },
            ExprKind::Load(ptr) => {
                // Process load operations
                let processed_ptr = self.process_expr(ptr);
                
                ExprKind::Load(Box::new(processed_ptr))
            },
            ExprKind::Addr(val) => {
                // Process address-of operations
                let processed_val = self.process_expr(val);
                
                ExprKind::Addr(Box::new(processed_val))
            },
            ExprKind::DataConstructor { tag, value } => {
                // Process data constructors
                let processed_value = if let Some(val) = value {
                    Some(Box::new(self.process_expr(val)))
                } else {
                    None
                };
                
                ExprKind::DataConstructor {
                    tag: tag.clone(),
                    value: processed_value,
                }
            },
            ExprKind::TypeCheck { value, check_type } => {
                // Process type checks
                let processed_value = self.process_expr(value);
                
                ExprKind::TypeCheck {
                    value: Box::new(processed_value),
                    check_type: check_type.clone(),
                }
            },
            // Return nodes are already explicit, so just clone them
            ExprKind::Return(val) => {
                let processed_val = self.process_expr(val);
                ExprKind::Return(Box::new(processed_val))
            },
            // Literals and symbols don't need processing
            _ => expr.node.clone(),
        };
        
        // Create a new expression with the processed node
        Located::new(processed_node, expr.span)
    }
    
    /// Process a block of expressions, making the last expression an explicit return
    fn process_block(&mut self, exprs: &[Expr], _span: Span) -> ExprKind {
        if exprs.is_empty() {
            // Empty block, return a unit value (or null)
            return ExprKind::Do(Vec::new());
        }
        
        let mut processed_exprs = Vec::with_capacity(exprs.len());
        
        // Process all but the last expression normally
        for expr in &exprs[..exprs.len() - 1] {
            processed_exprs.push(self.process_expr(expr));
        }
        
        // Make the last expression an explicit return
        let last_expr = &exprs[exprs.len() - 1];
        let processed_last = self.process_expr(last_expr);
        
        // Only wrap in a Return if it's not already a Return
        match &processed_last.node {
            ExprKind::Return(_) => {
                // Already a return, just add it to the block
                processed_exprs.push(processed_last);
            },
            _ => {
                // Wrap in a Return node
                let return_expr = Located::new(
                    ExprKind::Return(Box::new(processed_last)),
                    last_expr.span,
                );
                processed_exprs.push(return_expr);
            }
        }
        
        ExprKind::Do(processed_exprs)
    }
    
    /// Check if a type is generic
    fn is_generic_type(&self, ty: &Type) -> bool {
        match ty {
            Type::GenericStruct { .. } => true,
            Type::GenericTuple { .. } => true,
            Type::GenericData { .. } => true,
            Type::GenericInstance { .. } => true,
            _ => false,
        }
    }
    
    /// Collect all type definitions from the program
    fn collect_type_definitions(&mut self, program: &Program) {
        for form in &program.forms {
            if let TopLevelKind::TypeDef { name, ty } = &form.node {
                match ty {
                    Type::GenericStruct { .. } | 
                    Type::GenericTuple { .. } | 
                    Type::GenericData { .. } => {
                        self.generic_type_definitions.insert(name.clone(), ty.clone());
                    },
                    _ => {
                        self.type_definitions.insert(name.clone(), ty.clone());
                    }
                }
            }
        }
    }
    
    /// Collect all generic type instantiations in the program
    fn collect_generic_instances(&mut self, program: &Program) {
        for form in &program.forms {
            match &form.node {
                TopLevelKind::VarDef { value, .. } => {
                    self.find_generic_instances_in_expr(value);
                },
                TopLevelKind::Expr(expr_kind) => {
                    let expr = Located::new(expr_kind.clone(), Span::new(0, 0));
                    self.find_generic_instances_in_expr(&expr);
                },
                _ => {}
            }
        }
    }
    
    /// Find all generic instances used in an expression
    fn find_generic_instances_in_expr(&mut self, expr: &Expr) {
        match &expr.node {
            ExprKind::Call { name, args } => {
                // Check if this is a generic instance call like "vector<i32>"
                if let Some(idx) = name.find('<') {
                    if let Some(end_idx) = name.find('>') {
                        let base_name = &name[0..idx];
                        let type_args_str = &name[idx+1..end_idx];
                        
                        // Parse the type arguments
                        let type_args = type_args_str.split_whitespace()
                            .filter_map(|s| self.parse_type_arg(s))
                            .collect::<Vec<_>>();
                        
                        // Generate a mangled name
                        let mangled_name = self.mangle_name(base_name, &type_args);
                        
                        // Add to concrete instances if not already there
                        if !self.concrete_type_instances.contains(&mangled_name) {
                            self.concrete_type_instances.insert(mangled_name.clone());
                            
                            // Create monomorphized version
                            if let Some(generic_type) = self.generic_type_definitions.get(base_name) {
                                let concrete_type = self.monomorphize_type(generic_type, &type_args);
                                let mono_type = MonomorphizedType {
                                    original_name: base_name.to_string(),
                                    mangled_name,
                                    concrete_type,
                                    type_args,
                                };
                                self.monomorphized_types.insert(mono_type.mangled_name.clone(), mono_type);
                            }
                        }
                    }
                }
                
                // Recursively search in arguments
                for arg in args {
                    self.find_generic_instances_in_expr(arg);
                }
            },
            ExprKind::Binary { left, right, .. } => {
                self.find_generic_instances_in_expr(left);
                self.find_generic_instances_in_expr(right);
            },
            ExprKind::If { condition, then_branch, else_branch } => {
                self.find_generic_instances_in_expr(condition);
                self.find_generic_instances_in_expr(then_branch);
                if let Some(else_expr) = else_branch {
                    self.find_generic_instances_in_expr(else_expr);
                }
            },
            ExprKind::Do(exprs) => {
                for expr in exprs {
                    self.find_generic_instances_in_expr(expr);
                }
            },
            ExprKind::Return(val) => {
                self.find_generic_instances_in_expr(val);
            },
            ExprKind::Match { scrutinee, cases } => {
                self.find_generic_instances_in_expr(scrutinee);
                for case in cases {
                    self.find_generic_instances_in_expr(&case.result);
                }
            },
            ExprKind::For { iterator, body } => {
                self.find_generic_instances_in_expr(body);
                if let Some(iter) = iterator {
                    match &**iter {
                        crate::ast::ForIterator::Condition(cond) => {
                            self.find_generic_instances_in_expr(cond);
                        },
                        crate::ast::ForIterator::Range { collection, .. } => {
                            self.find_generic_instances_in_expr(collection);
                        },
                    }
                }
            },
            ExprKind::ModuleCall { args, .. } => {
                for arg in args {
                    self.find_generic_instances_in_expr(arg);
                }
            },
            ExprKind::FieldAccess { object, .. } => {
                self.find_generic_instances_in_expr(object);
            },
            ExprKind::SetField { object, value, .. } => {
                self.find_generic_instances_in_expr(object);
                self.find_generic_instances_in_expr(value);
            },
            ExprKind::SetIndex { array, index, value } => {
                self.find_generic_instances_in_expr(array);
                self.find_generic_instances_in_expr(index);
                self.find_generic_instances_in_expr(value);
            },
            ExprKind::Store { addr, value } => {
                self.find_generic_instances_in_expr(addr);
                self.find_generic_instances_in_expr(value);
            },
            ExprKind::Load(ptr) => {
                self.find_generic_instances_in_expr(ptr);
            },
            ExprKind::Addr(val) => {
                self.find_generic_instances_in_expr(val);
            },
            ExprKind::DataConstructor { value, .. } => {
                if let Some(val) = value {
                    self.find_generic_instances_in_expr(val);
                }
            },
            ExprKind::TypeCheck { value, .. } => {
                self.find_generic_instances_in_expr(value);
            },
            _ => {},
        }
    }
    
    /// Parse a type argument from a string
    fn parse_type_arg(&self, s: &str) -> Option<Type> {
        match s {
            "i8" => Some(Type::Int(8)),
            "i16" => Some(Type::Int(16)),
            "i32" => Some(Type::Int(32)),
            "i64" => Some(Type::Int(64)),
            "i128" => Some(Type::Int(128)),
            "u8" => Some(Type::UInt(8)),
            "u16" => Some(Type::UInt(16)),
            "u32" => Some(Type::UInt(32)),
            "u64" => Some(Type::UInt(64)),
            "u128" => Some(Type::UInt(128)),
            "f32" => Some(Type::Float(32)),
            "f64" => Some(Type::Float(64)),
            "bool" => Some(Type::Bool),
            "atom" => Some(Type::Atom),
            _ => {
                // Check if it's a defined type alias
                self.type_definitions.get(s).cloned()
            }
        }
    }
    
    /// Create a mangled name for a generic instantiation
    fn mangle_name(&self, base: &str, type_args: &[Type]) -> String {
        let mut result = base.to_string();
        result.push('_');
        
        for (i, arg) in type_args.iter().enumerate() {
            if i > 0 {
                result.push('_');
            }
            
            // Convert the type to a string suitable for a mangled name
            match arg {
                Type::Int(bits) => result.push_str(&format!("i{}", bits)),
                Type::UInt(bits) => result.push_str(&format!("u{}", bits)),
                Type::Float(bits) => result.push_str(&format!("f{}", bits)),
                Type::Bool => result.push_str("bool"),
                Type::Atom => result.push_str("atom"),
                Type::Pointer(inner) => {
                    result.push_str("ptr_");
                    // Simplified for now, we could recursively mangle the inner type
                    result.push_str(&format!("{:?}", inner));
                },
                Type::Named(name) => result.push_str(name),
                _ => {
                    // For complex types, use a hash or simplified representation
                    result.push_str(&format!("complex{}", type_args.len()));
                }
            }
        }
        
        result
    }
    
    /// Monomorphize a generic type with concrete type arguments
    fn monomorphize_type(&self, generic_type: &Type, type_args: &[Type]) -> Type {
        match generic_type {
            Type::GenericStruct { type_params, fields } => {
                if type_params.len() != type_args.len() {
                    // Error: wrong number of type arguments
                    return Type::Named("error_wrong_type_args".to_string());
                }
                
                // Create parameter to argument mapping
                let mut type_map = HashMap::new();
                for (param, arg) in type_params.iter().zip(type_args.iter()) {
                    type_map.insert(param.clone(), arg.clone());
                }
                
                // Substitute parameters in fields
                let concrete_fields = fields.iter()
                    .map(|(name, field_type)| {
                        (name.clone(), self.substitute_type_params(&type_map, field_type))
                    })
                    .collect();
                
                Type::Struct(concrete_fields)
            },
            Type::GenericTuple { type_params, types } => {
                if type_params.len() != type_args.len() {
                    return Type::Named("error_wrong_type_args".to_string());
                }
                
                // Create parameter to argument mapping
                let mut type_map = HashMap::new();
                for (param, arg) in type_params.iter().zip(type_args.iter()) {
                    type_map.insert(param.clone(), arg.clone());
                }
                
                // Substitute parameters in tuple types
                let concrete_types = types.iter()
                    .map(|t| self.substitute_type_params(&type_map, t))
                    .collect();
                
                Type::Tuple(concrete_types)
            },
            Type::GenericData { type_params, variants } => {
                if type_params.len() != type_args.len() {
                    return Type::Named("error_wrong_type_args".to_string());
                }
                
                // Create parameter to argument mapping
                let mut type_map = HashMap::new();
                for (param, arg) in type_params.iter().zip(type_args.iter()) {
                    type_map.insert(param.clone(), arg.clone());
                }
                
                // Substitute parameters in variants
                let concrete_variants = variants.iter()
                    .map(|(tag, variant_type_opt)| {
                        (
                            tag.clone(),
                            variant_type_opt.as_ref().map(|t| self.substitute_type_params(&type_map, t))
                        )
                    })
                    .collect();
                
                Type::Data(concrete_variants)
            },
            _ => generic_type.clone(),
        }
    }
    
    /// Substitute type parameters in a type
    fn substitute_type_params(&self, type_map: &HashMap<String, Type>, ty: &Type) -> Type {
        match ty {
            Type::Named(name) => {
                // If it's a type parameter, replace with concrete type
                if let Some(concrete_type) = type_map.get(name) {
                    concrete_type.clone()
                } else {
                    ty.clone()
                }
            },
            Type::Pointer(inner) => {
                Type::Pointer(Box::new(self.substitute_type_params(type_map, inner)))
            },
            Type::Array(inner, size) => {
                Type::Array(Box::new(self.substitute_type_params(type_map, inner)), *size)
            },
            Type::Function(param_types, return_type) => {
                let new_param_types = param_types.iter()
                    .map(|pt| self.substitute_type_params(type_map, pt))
                    .collect();
                let new_return_type = Box::new(self.substitute_type_params(type_map, return_type));
                Type::Function(new_param_types, new_return_type)
            },
            Type::Struct(fields) => {
                let new_fields = fields.iter()
                    .map(|(name, field_type)| {
                        (name.clone(), self.substitute_type_params(type_map, field_type))
                    })
                    .collect();
                Type::Struct(new_fields)
            },
            Type::Tuple(types) => {
                let new_types = types.iter()
                    .map(|t| self.substitute_type_params(type_map, t))
                    .collect();
                Type::Tuple(new_types)
            },
            Type::Data(variants) => {
                let new_variants = variants.iter()
                    .map(|(tag, variant_type_opt)| {
                        (
                            tag.clone(),
                            variant_type_opt.as_ref().map(|t| self.substitute_type_params(type_map, t))
                        )
                    })
                    .collect();
                Type::Data(new_variants)
            },
            Type::Union(types) => {
                let new_types = types.iter()
                    .map(|t| self.substitute_type_params(type_map, t))
                    .collect();
                Type::Union(new_types)
            },
            _ => ty.clone(),
        }
    }
} 