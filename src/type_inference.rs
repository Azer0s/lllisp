use std::collections::HashMap;
use crate::ast::{
    Expr, ExprKind, Literal, Program, TopLevel, TopLevelKind, Type, Located, Span, BinaryOp
};

/// A symbol table to keep track of type information
#[derive(Debug, Clone)]
pub struct SymbolTable {
    // Map of variable names to their types
    variables: HashMap<String, Type>,
    // Map of type alias names to their actual types
    type_aliases: HashMap<String, Type>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            type_aliases: HashMap::new(),
        }
    }
    
    pub fn add_variable(&mut self, name: &str, ty: Type) {
        self.variables.insert(name.to_string(), ty);
    }
    
    pub fn add_type_alias(&mut self, name: &str, ty: Type) {
        self.type_aliases.insert(name.to_string(), ty);
    }
    
    pub fn get_variable_type(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }
    
    pub fn resolve_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Named(name) => {
                if let Some(resolved) = self.type_aliases.get(name) {
                    // Recursively resolve type aliases
                    self.resolve_type(resolved)
                } else {
                    // Keep the named type if not an alias
                    ty.clone()
                }
            },
            Type::Pointer(inner) => {
                Type::Pointer(Box::new(self.resolve_type(inner)))
            },
            Type::Array(inner, size) => {
                Type::Array(Box::new(self.resolve_type(inner)), *size)
            },
            Type::GenericInstance { base, type_args } => {
                // Try to resolve the base type
                if let Some(base_type) = self.type_aliases.get(base) {
                    // Apply type arguments to the generic type
                    self.apply_type_args(base_type, type_args)
                } else {
                    // Keep as is if the base type is not found
                    ty.clone()
                }
            },
            _ => ty.clone(),
        }
    }
    
    // Apply type arguments to a generic type
    fn apply_type_args(&self, base_type: &Type, type_args: &[Type]) -> Type {
        match base_type {
            Type::GenericStruct { type_params, fields } => {
                if type_params.len() != type_args.len() {
                    // Wrong number of type arguments, return as is
                    return Type::GenericInstance {
                        base: format!("{:?}", base_type),
                        type_args: type_args.to_vec(),
                    };
                }
                
                // Create a mapping from type parameters to concrete types
                let mut type_map = HashMap::new();
                for (param, arg) in type_params.iter().zip(type_args.iter()) {
                    type_map.insert(param.clone(), arg.clone());
                }
                
                // Replace type parameters in fields
                let substituted_fields = fields.iter()
                    .map(|(name, field_type)| {
                        (name.clone(), self.substitute_type_params(&type_map, field_type))
                    })
                    .collect();
                
                Type::Struct(substituted_fields)
            },
            Type::GenericTuple { type_params, types } => {
                if type_params.len() != type_args.len() {
                    // Wrong number of type arguments, return as is
                    return Type::GenericInstance {
                        base: format!("{:?}", base_type),
                        type_args: type_args.to_vec(),
                    };
                }
                
                // Create a mapping from type parameters to concrete types
                let mut type_map = HashMap::new();
                for (param, arg) in type_params.iter().zip(type_args.iter()) {
                    type_map.insert(param.clone(), arg.clone());
                }
                
                // Replace type parameters in tuple types
                let substituted_types = types.iter()
                    .map(|t| self.substitute_type_params(&type_map, t))
                    .collect();
                
                Type::Tuple(substituted_types)
            },
            Type::GenericData { type_params, variants } => {
                if type_params.len() != type_args.len() {
                    // Wrong number of type arguments, return as is
                    return Type::GenericInstance {
                        base: format!("{:?}", base_type),
                        type_args: type_args.to_vec(),
                    };
                }
                
                // Create a mapping from type parameters to concrete types
                let mut type_map = HashMap::new();
                for (param, arg) in type_params.iter().zip(type_args.iter()) {
                    type_map.insert(param.clone(), arg.clone());
                }
                
                // Replace type parameters in variants
                let substituted_variants = variants.iter()
                    .map(|(tag, variant_type_opt)| {
                        (
                            tag.clone(),
                            variant_type_opt.as_ref().map(|t| self.substitute_type_params(&type_map, t))
                        )
                    })
                    .collect();
                
                Type::Data(substituted_variants)
            },
            _ => Type::GenericInstance {
                base: format!("{:?}", base_type),
                type_args: type_args.to_vec(),
            },
        }
    }
    
    // Substitute type parameters in a type
    fn substitute_type_params(&self, type_map: &HashMap<String, Type>, ty: &Type) -> Type {
        match ty {
            Type::Named(name) => {
                // If it's a type parameter, replace with the concrete type
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

/// Error during type checking
#[derive(Debug)]
pub enum TypeError {
    UndefinedVariable(String, Span),
    UndefinedType(String, Span),
    TypeMismatch { expected: Type, found: Type, span: Span },
    AddressOfNonAddressable(Span),
    DerefOfNonPointer(Span),
    FieldNotFound { struct_type: Type, field: String, span: Span },
    NotAStruct { ty: Type, span: Span },
    NotAnArray { ty: Type, span: Span },
    InvalidIndexType { ty: Type, span: Span },
    InvalidDataTag { data_type: Type, tag: String, span: Span },
    InvalidDataValue { expected: Type, found: Type, span: Span },
    NotADataType { ty: Type, span: Span },
    InvalidTupleIndex { index: usize, tuple_size: usize, span: Span },
    InvalidModulePath { module: String, reason: String, span: Span },
}

/// Type inference engine
pub struct TypeInferer {
    symbol_table: SymbolTable,
}

impl TypeInferer {
    pub fn new() -> Self {
        let mut inferer = Self {
            symbol_table: SymbolTable::new(),
        };
        
        // Register special operators
        inferer.symbol_table.add_variable("addr", Type::Function(
            vec![], 
            Box::new(Type::Pointer(Box::new(Type::Named("any".to_string()))))
        ));
        
        // Register the type check operator
        inferer.symbol_table.add_variable("is", Type::Function(
            vec![Type::Named("any".to_string()), Type::Named("any".to_string())], 
            Box::new(Type::Bool)
        ));
        
        // Register basic types as variables in the symbol table
        // This way, they can be used as type constructors like (i32 43)
        
        // Integer types
        inferer.symbol_table.add_variable("i8", Type::Int(8));
        inferer.symbol_table.add_variable("i16", Type::Int(16));
        inferer.symbol_table.add_variable("i32", Type::Int(32));
        inferer.symbol_table.add_variable("i64", Type::Int(64));
        inferer.symbol_table.add_variable("i128", Type::Int(128));
        
        // Unsigned integer types
        inferer.symbol_table.add_variable("u8", Type::UInt(8));
        inferer.symbol_table.add_variable("u16", Type::UInt(16));
        inferer.symbol_table.add_variable("u32", Type::UInt(32));
        inferer.symbol_table.add_variable("u64", Type::UInt(64));
        inferer.symbol_table.add_variable("u128", Type::UInt(128));
        
        // Floating point types
        inferer.symbol_table.add_variable("f16", Type::Float(16));
        inferer.symbol_table.add_variable("f32", Type::Float(32));
        inferer.symbol_table.add_variable("f64", Type::Float(64));
        inferer.symbol_table.add_variable("f128", Type::Float(128));
        
        // Other basic types
        inferer.symbol_table.add_variable("bool", Type::Bool);
        inferer.symbol_table.add_variable("atom", Type::Atom);
        inferer.symbol_table.add_variable("ptr", Type::Named("ptr".to_string()));
        
        inferer
    }
    
    /// Get the type of a variable
    pub fn get_variable_type(&self, name: &str) -> Option<&Type> {
        self.symbol_table.get_variable_type(name)
    }
    
    /// Helper method to check if a name corresponds to a basic type
    fn get_basic_type_from_name(&self, name: &str) -> Option<Type> {
        match name {
            // Integer types
            "i8" => Some(Type::Int(8)),
            "i16" => Some(Type::Int(16)),
            "i32" => Some(Type::Int(32)),
            "i64" => Some(Type::Int(64)),
            "i128" => Some(Type::Int(128)),
            
            // Unsigned integer types
            "u8" => Some(Type::UInt(8)),
            "u16" => Some(Type::UInt(16)),
            "u32" => Some(Type::UInt(32)),
            "u64" => Some(Type::UInt(64)),
            "u128" => Some(Type::UInt(128)),
            
            // Floating point types
            "f16" => Some(Type::Float(16)),
            "f32" => Some(Type::Float(32)),
            "f64" => Some(Type::Float(64)),
            "f128" => Some(Type::Float(128)),
            
            // Pointer type (special case, handled differently)
            "ptr" => Some(Type::Named("ptr".to_string())),
            
            // Other basic types
            "bool" => Some(Type::Bool),
            "atom" => Some(Type::Atom),
            
            // Not a basic type
            _ => None,
        }
    }
    
    /// Process a program and perform type inference
    pub fn process_program(&mut self, program: &Program) -> Result<Program, Vec<TypeError>> {
        let mut errors = Vec::new();
        let mut processed_forms = Vec::new();
        
        // First pass: collect type aliases
        for form in &program.forms {
            if let TopLevelKind::TypeDef { name, ty } = &form.node {
                // Add the type alias to the symbol table
                self.symbol_table.add_type_alias(name, ty.clone());
            }
        }
        
        // Second pass: process all forms
        for form in &program.forms {
            match self.process_top_level(form) {
                Ok(processed) => processed_forms.push(processed),
                Err(err) => errors.push(err),
            }
        }
        
        if errors.is_empty() {
            Ok(Program { forms: processed_forms })
        } else {
            Err(errors)
        }
    }
    
    /// Process a top-level form
    fn process_top_level(&mut self, form: &TopLevel) -> Result<TopLevel, TypeError> {
        match &form.node {
            TopLevelKind::TypeDef { name, ty } => {
                // Register the type alias
                self.symbol_table.add_type_alias(name, ty.clone());
                
                // Return the type definition unchanged
                Ok(form.clone())
            },
            TopLevelKind::VarDef { name, value } => {
                // Infer the type of the value expression
                let typed_value = self.infer_expr_type(value)?;
                
                // Infer the type from the expression
                let inferred_type = self.infer_type_from_expr(&typed_value)?;
                
                // Register the variable with its type
                self.symbol_table.add_variable(name, inferred_type);
                
                // Return the variable definition with the typed expression
                Ok(Located::new(
                    TopLevelKind::VarDef {
                        name: name.clone(),
                        value: typed_value,
                    },
                    form.span,
                ))
            },
            TopLevelKind::ModuleImport { name, path, is_header } => {
                // For now, we don't do any type checking on module imports
                // In a full implementation, we would load the module's interface here
                
                // Register the module name in the symbol table
                // This allows us to track that this is a valid module name
                // We use a special type to represent modules
                let module_type = if *is_header {
                    Type::Named(format!("module:{}:header:{}", path, name))
                } else {
                    Type::Named(format!("module:{}", path))
                };
                self.symbol_table.add_variable(name, module_type);
                
                // Return the module import unchanged
                Ok(form.clone())
            },
            TopLevelKind::Expr(expr) => {
                // Infer the type of the expression
                let typed_expr = self.infer_expr_type(&Located::new(expr.clone(), form.span))?;
                
                // Return the expression with its inferred type
                Ok(Located::new(
                    TopLevelKind::Expr(typed_expr.node),
                    form.span,
                ))
            },
            TopLevelKind::MacroDef { name, params, body } => {
                // For now, we don't do much type checking on macros
                // In a full implementation, we would analyze the macro body
                
                // Return the macro definition unchanged
                Ok(form.clone())
            },
        }
    }
    
    /// Infer types for an expression
    fn infer_expr_type(&mut self, expr: &Expr) -> Result<Expr, TypeError> {
        match &expr.node {
            ExprKind::Literal(literal) => {
                Ok(expr.clone())
            },
            ExprKind::Symbol(name) => {
                if let Some(_var_type) = self.symbol_table.get_variable_type(name) {
                    Ok(expr.clone())
                } else {
                    Err(TypeError::UndefinedVariable(name.clone(), expr.span))
                }
            },
            ExprKind::Addr(inner) => {
                let processed_inner = self.infer_expr_type(inner)?;
                Ok(Located::new(ExprKind::Addr(Box::new(processed_inner)), expr.span))
            },
            ExprKind::Load(inner) => {
                let processed_inner = self.infer_expr_type(inner)?;
                let inner_type = self.infer_type_from_expr(&processed_inner)?;
                match inner_type {
                    Type::Pointer(_) => Ok(Located::new(ExprKind::Load(Box::new(processed_inner)), expr.span)),
                    _ => Err(TypeError::DerefOfNonPointer(expr.span)),
                }
            },
            ExprKind::Store { addr, value } => {
                let processed_addr = self.infer_expr_type(addr)?;
                let processed_value = self.infer_expr_type(value)?;
                let addr_type = self.infer_type_from_expr(&processed_addr)?;
                let value_type = self.infer_type_from_expr(&processed_value)?;
                match addr_type {
                    Type::Pointer(ref pointed_to) if **pointed_to == value_type =>
                        Ok(Located::new(ExprKind::Store { addr: Box::new(processed_addr), value: Box::new(processed_value) }, expr.span)),
                    Type::Pointer(pointed_to) => Err(TypeError::TypeMismatch { expected: *pointed_to, found: value_type, span: expr.span }),
                    _ => Err(TypeError::DerefOfNonPointer(expr.span)),
                }
            },
            ExprKind::FieldAccess { object, field } => {
                let processed_object = self.infer_expr_type(object)?;
                let object_type = self.infer_type_from_expr(&processed_object)?;
                
                // Resolve the object type if it's a named type
                let resolved_type = self.symbol_table.resolve_type(&object_type);
                
                match resolved_type {
                    Type::Struct(fields) => {
                        // Find the field in the struct
                        if let Some((_, field_type)) = fields.iter().find(|(name, _)| name == field) {
                            Ok(Located::new(
                                ExprKind::FieldAccess {
                                    object: Box::new(processed_object),
                                    field: field.clone(),
                                },
                                expr.span
                            ))
                        } else {
                            Err(TypeError::FieldNotFound {
                                struct_type: object_type,
                                field: field.clone(),
                                span: expr.span,
                            })
                        }
                    },
                    _ => Err(TypeError::NotAStruct {
                        ty: object_type,
                        span: expr.span,
                    }),
                }
            },
            ExprKind::SetField { object, field, value } => {
                let processed_object = self.infer_expr_type(object)?;
                let processed_value = self.infer_expr_type(value)?;
                let object_type = self.infer_type_from_expr(&processed_object)?;
                let value_type = self.infer_type_from_expr(&processed_value)?;
                
                // Resolve the object type if it's a named type
                let resolved_type = self.symbol_table.resolve_type(&object_type);
                
                match resolved_type {
                    Type::Struct(fields) => {
                        // Find the field in the struct
                        if let Some((_, field_type)) = fields.iter().find(|(name, _)| name == field) {
                            // Check if the value type matches the field type
                            if *field_type == value_type {
                                Ok(Located::new(
                                    ExprKind::SetField {
                                        object: Box::new(processed_object),
                                        field: field.clone(),
                                        value: Box::new(processed_value),
                                    },
                                    expr.span
                                ))
                            } else {
                                Err(TypeError::TypeMismatch {
                                    expected: field_type.clone(),
                                    found: value_type,
                                    span: expr.span,
                                })
                            }
                        } else {
                            Err(TypeError::FieldNotFound {
                                struct_type: object_type,
                                field: field.clone(),
                                span: expr.span,
                            })
                        }
                    },
                    _ => Err(TypeError::NotAStruct {
                        ty: object_type,
                        span: expr.span,
                    }),
                }
            },
            ExprKind::SetIndex { array, index, value } => {
                let processed_array = self.infer_expr_type(array)?;
                let processed_index = self.infer_expr_type(index)?;
                let processed_value = self.infer_expr_type(value)?;
                
                let array_type = self.infer_type_from_expr(&processed_array)?;
                let index_type = self.infer_type_from_expr(&processed_index)?;
                let value_type = self.infer_type_from_expr(&processed_value)?;
                
                // Check if index is an integer type
                match index_type {
                    Type::Int(_) | Type::UInt(_) => {},
                    _ => return Err(TypeError::InvalidIndexType {
                        ty: index_type,
                        span: index.span,
                    }),
                }
                
                // Check if array is an array type
                match array_type {
                    Type::Array(element_type, _) => {
                        // Check if value type matches element type
                        if *element_type == value_type {
                            Ok(Located::new(
                                ExprKind::SetIndex {
                                    array: Box::new(processed_array),
                                    index: Box::new(processed_index),
                                    value: Box::new(processed_value),
                                },
                                expr.span
                            ))
                        } else {
                            Err(TypeError::TypeMismatch {
                                expected: *element_type,
                                found: value_type,
                                span: expr.span,
                            })
                        }
                    },
                    _ => Err(TypeError::NotAnArray {
                        ty: array_type,
                        span: expr.span,
                    }),
                }
            },
            ExprKind::SetAddr { addr, value } => {
                let processed_addr = self.infer_expr_type(addr)?;
                let processed_value = self.infer_expr_type(value)?;
                
                // For raw memory access, we don't check types as strictly
                Ok(Located::new(
                    ExprKind::SetAddr {
                        addr: Box::new(processed_addr),
                        value: Box::new(processed_value),
                    },
                    expr.span
                ))
            },
            ExprKind::Call { name, args } => {
                // Special case for the "is" type check operator
                if name == "is" && args.len() == 2 {
                    // args[0] should be a Type name as a Symbol
                    if let ExprKind::Symbol(type_name) = &args[0].node {
                        // Convert the type name to a Type
                        let type_to_check = if let Some(basic_type) = self.get_basic_type_from_name(type_name) {
                            basic_type
                        } else if let Some(ty) = self.symbol_table.get_variable_type(type_name) {
                            ty.clone()
                        } else {
                            return Err(TypeError::UndefinedType(type_name.clone(), args[0].span));
                        };
                        
                        // Process the value to check
                        let processed_value = self.infer_expr_type(&args[1])?;
                        
                        return Ok(Located::new(
                            ExprKind::TypeCheck {
                                value: Box::new(processed_value),
                                check_type: type_to_check,
                            },
                            expr.span
                        ));
                    }
                }
                
                // Check if this is a type constructor call
                // e.g., (i32 43) or (ptr i32 (addr x))
                if let Some(basic_type) = self.get_basic_type_from_name(name) {
                    // Process args
                    let mut processed_args = Vec::new();
                    for arg in args {
                        let processed_arg = self.infer_expr_type(arg)?;
                        processed_args.push(processed_arg);
                    }
                    
                    // For primitive type constructors, validate the value
                    match &basic_type {
                        Type::Int(_) | Type::UInt(_) => {
                            if args.len() != 1 {
                                return Err(TypeError::TypeMismatch { 
                                    expected: basic_type.clone(), 
                                    found: Type::Named("invalid-args".to_string()), 
                                    span: expr.span 
                                });
                            }
                            
                            // Check if the argument is a numeric literal
                            let arg_type = self.infer_type_from_expr(&processed_args[0])?;
                            match arg_type {
                                Type::Int(_) | Type::UInt(_) | Type::Float(_) => {},
                                _ => return Err(TypeError::TypeMismatch { 
                                    expected: basic_type.clone(), 
                                    found: arg_type, 
                                    span: args[0].span 
                                }),
                            }
                        },
                        Type::Float(_) => {
                            if args.len() != 1 {
                                return Err(TypeError::TypeMismatch { 
                                    expected: basic_type.clone(), 
                                    found: Type::Named("invalid-args".to_string()), 
                                    span: expr.span 
                                });
                            }
                            
                            // Check if the argument is a numeric literal
                            let arg_type = self.infer_type_from_expr(&processed_args[0])?;
                            match arg_type {
                                Type::Int(_) | Type::UInt(_) | Type::Float(_) => {},
                                _ => return Err(TypeError::TypeMismatch { 
                                    expected: basic_type.clone(), 
                                    found: arg_type, 
                                    span: args[0].span 
                                }),
                            }
                        },
                        Type::Pointer(_) => {
                            if args.len() != 1 {
                                return Err(TypeError::TypeMismatch { 
                                    expected: basic_type.clone(), 
                                    found: Type::Named("invalid-args".to_string()), 
                                    span: expr.span 
                                });
                            }
                        },
                        _ => {
                            // For other types, just pass through
                        }
                    }
                    
                    // Return the type constructor call
                    return Ok(Located::new(
                        ExprKind::Call {
                            name: name.clone(),
                            args: processed_args,
                        },
                        expr.span
                    ));
                }
                
                // Check if this is a named type constructor
                if let Some(ty) = self.symbol_table.get_variable_type(name) {
                    // Process args
                    let mut processed_args = Vec::new();
                    for arg in args {
                        let processed_arg = self.infer_expr_type(arg)?;
                        processed_args.push(processed_arg);
                    }
                    
                    // Return the type constructor call
                    return Ok(Located::new(
                        ExprKind::Call {
                            name: name.clone(),
                            args: processed_args,
                        },
                        expr.span
                    ));
                }
                
                // Handle regular function calls
                let mut processed_args = Vec::new();
                for arg in args {
                    let processed_arg = self.infer_expr_type(arg)?;
                    processed_args.push(processed_arg);
                }
                
                Ok(Located::new(
                    ExprKind::Call {
                        name: name.clone(),
                        args: processed_args,
                    },
                    expr.span
                ))
            },
            ExprKind::Do(expressions) => {
                let mut processed_expressions = Vec::new();
                for exp in expressions {
                    let processed_expr = self.infer_expr_type(exp)?;
                    processed_expressions.push(processed_expr);
                }
                Ok(Located::new(ExprKind::Do(processed_expressions), expr.span))
            },
            ExprKind::If { condition, then_branch, else_branch } => {
                let processed_condition = self.infer_expr_type(condition)?;
                let processed_then = self.infer_expr_type(then_branch)?;
                let then_type = self.infer_type_from_expr(&processed_then)?;
                let processed_else = if let Some(else_expr) = else_branch {
                    let processed = self.infer_expr_type(else_expr)?;
                    let else_type = self.infer_type_from_expr(&processed)?;
                    if else_type != then_type {
                        return Err(TypeError::TypeMismatch { expected: then_type.clone(), found: else_type, span: expr.span });
                    }
                    Some(Box::new(processed))
                } else {
                    None
                };
                Ok(Located::new(ExprKind::If { condition: Box::new(processed_condition), then_branch: Box::new(processed_then), else_branch: processed_else }, expr.span))
            },
            ExprKind::For { iterator, body } => {
                // Process the for loop body
                let processed_body = self.infer_expr_type(body)?;
                
                // Process the iterator if present
                let processed_iterator = if let Some(iter) = iterator {
                    match &**iter {
                        crate::ast::ForIterator::Condition(condition) => {
                            // Process the condition
                            let processed_condition = self.infer_expr_type(condition)?;
                            // Condition should be boolean
                            let condition_type = self.infer_type_from_expr(&processed_condition)?;
                            if condition_type != Type::Bool {
                                return Err(TypeError::TypeMismatch { 
                                    expected: Type::Bool, 
                                    found: condition_type, 
                                    span: condition.span 
                                });
                            }
                            Some(Box::new(crate::ast::ForIterator::Condition(processed_condition)))
                        },
                        crate::ast::ForIterator::Range { var, collection } => {
                            // Process the collection expression
                            let processed_collection = self.infer_expr_type(collection)?;
                            
                            // Create a temporary variable for the iterator variable
                            // This could be done more sophisticatedly with a dedicated iterator type
                            // For now, we'll assume the iterator variable is of type determined by the collection
                            let var_type = Type::Named("auto".to_string()); // Placeholder type
                            self.symbol_table.add_variable(var, var_type);
                            
                            Some(Box::new(crate::ast::ForIterator::Range {
                                var: var.clone(),
                                collection: processed_collection,
                            }))
                        },
                    }
                } else {
                    None
                };
                
                Ok(Located::new(ExprKind::For { 
                    iterator: processed_iterator,
                    body: Box::new(processed_body),
                }, expr.span))
            },
            ExprKind::Binary { op, left, right } => {
                let processed_left = self.infer_expr_type(left)?;
                let processed_right = self.infer_expr_type(right)?;
                Ok(Located::new(ExprKind::Binary { op: op.clone(), left: Box::new(processed_left), right: Box::new(processed_right) }, expr.span))
            },
            ExprKind::Literal(Literal::Tuple(elements)) => {
                let mut processed_elements = Vec::new();
                for elem in elements {
                    let processed_elem = self.infer_expr_type(elem)?;
                    processed_elements.push(processed_elem);
                }
                Ok(Located::new(ExprKind::Literal(Literal::Tuple(processed_elements)), expr.span))
            },
            ExprKind::DataConstructor { tag, value } => {
                // Find a data type that contains this tag
                let mut matching_type = None;
                for (type_name, ty) in &self.symbol_table.variables {
                    if let Type::Data(variants) = ty {
                        if variants.iter().any(|(variant_tag, _)| variant_tag == tag) {
                            matching_type = Some((type_name.clone(), ty.clone()));
                            break;
                        }
                    }
                }
                
                if let Some((_, data_type)) = matching_type {
                    if let Type::Data(variants) = &data_type {
                        // Find the specific variant
                        if let Some((_, value_type_opt)) = variants.iter().find(|(variant_tag, _)| variant_tag == tag) {
                            // Check if the value matches the expected type
                            match (value, value_type_opt) {
                                // Variant expects a value and we have one
                                (Some(value_expr), Some(expected_type)) => {
                                    let processed_value = self.infer_expr_type(value_expr)?;
                                    let value_type = self.infer_type_from_expr(&processed_value)?;
                                    
                                    if value_type != *expected_type {
                                        return Err(TypeError::InvalidDataValue {
                                            expected: expected_type.clone(),
                                            found: value_type,
                                            span: expr.span,
                                        });
                                    }
                                    
                                    Ok(Located::new(
                                        ExprKind::DataConstructor {
                                            tag: tag.clone(),
                                            value: Some(Box::new(processed_value)),
                                        },
                                        expr.span
                                    ))
                                },
                                // Variant doesn't expect a value and we don't have one
                                (None, None) => {
                                    Ok(Located::new(
                                        ExprKind::DataConstructor {
                                            tag: tag.clone(),
                                            value: None,
                                        },
                                        expr.span
                                    ))
                                },
                                // Mismatch between variant definition and usage
                                (Some(_), None) => {
                                    Err(TypeError::InvalidDataValue {
                                        expected: Type::Named("void".to_string()),
                                        found: Type::Named("any".to_string()),
                                        span: expr.span,
                                    })
                                },
                                (None, Some(expected_type)) => {
                                    Err(TypeError::InvalidDataValue {
                                        expected: expected_type.clone(),
                                        found: Type::Named("void".to_string()),
                                        span: expr.span,
                                    })
                                },
                            }
                        } else {
                            Err(TypeError::InvalidDataTag {
                                data_type: data_type.clone(),
                                tag: tag.clone(),
                                span: expr.span,
                            })
                        }
                    } else {
                        Err(TypeError::NotADataType {
                            ty: data_type.clone(),
                            span: expr.span,
                        })
                    }
                } else {
                    Err(TypeError::InvalidDataTag {
                        data_type: Type::Named("unknown".to_string()),
                        tag: tag.clone(),
                        span: expr.span,
                    })
                }
            },
            ExprKind::Match { scrutinee, cases } => {
                // Process the scrutinee expression
                let processed_scrutinee = self.infer_expr_type(scrutinee)?;
                let scrutinee_type = self.infer_type_from_expr(&processed_scrutinee)?;
                
                // Process each case
                let mut processed_cases = Vec::new();
                let mut result_type: Option<Type> = None;
                
                for case in cases {
                    // Process patterns (register potential bindings in the symbol table)
                    let processed_pattern = self.process_pattern(&case.pattern, &scrutinee_type)?;
                    
                    // Process the result expression
                    let processed_result = self.infer_expr_type(&case.result)?;
                    let case_result_type = self.infer_type_from_expr(&processed_result)?;
                    
                    // Check result type consistency across all cases
                    if let Some(expected_type) = &result_type {
                        if *expected_type != case_result_type {
                            return Err(TypeError::TypeMismatch {
                                expected: expected_type.clone(),
                                found: case_result_type,
                                span: case.result.span,
                            });
                        }
                    } else {
                        result_type = Some(case_result_type);
                    }
                    
                    processed_cases.push(crate::ast::MatchCase {
                        pattern: processed_pattern,
                        result: processed_result,
                    });
                }
                
                Ok(Located::new(
                    ExprKind::Match {
                        scrutinee: Box::new(processed_scrutinee),
                        cases: processed_cases,
                    },
                    expr.span
                ))
            },
            ExprKind::TypeCheck { value, check_type } => {
                // Process the value expression
                let processed_value = self.infer_expr_type(value)?;
                
                // Resolve the check_type if it's a named type
                let resolved_check_type = self.symbol_table.resolve_type(check_type);
                
                // Type checking always returns a boolean
                Ok(Located::new(
                    ExprKind::TypeCheck {
                        value: Box::new(processed_value),
                        check_type: resolved_check_type,
                    },
                    expr.span
                ))
            },
            ExprKind::ModuleCall { module, function, args } => {
                // Extract the root module (in case we have nested modules like "module/submodule")
                let root_module = if let Some(slash_pos) = module.find('/') {
                    &module[0..slash_pos]
                } else {
                    module
                };
                
                // Check if the root module exists
                if let Some(module_type) = self.symbol_table.get_variable_type(root_module) {
                    // Verify this is a module type
                    let is_module = match module_type {
                        Type::Named(name) => name.starts_with("module:"),
                        _ => false
                    };
                    
                    if !is_module {
                        return Err(TypeError::TypeMismatch {
                            expected: Type::Named("module".to_string()),
                            found: module_type.clone(),
                            span: expr.span,
                        });
                    }

                    // Check if this is a header module (C import)
                    let is_header_module = match module_type {
                        Type::Named(name) => name.starts_with("module:") && name.contains(":header:"),
                        _ => false
                    };

                    // If it's a header module, ensure there are no nested paths
                    if is_header_module && module.contains('/') {
                        return Err(TypeError::InvalidModulePath {
                            module: module.clone(),
                            reason: "C header imports cannot use nested module paths".to_string(),
                            span: expr.span,
                        });
                    }
                    
                    // Process the arguments
                    let mut processed_args = Vec::new();
                    for arg in args {
                        processed_args.push(self.infer_expr_type(arg)?);
                    }
                    
                    Ok(Located::new(
                        ExprKind::ModuleCall {
                            module: module.clone(),
                            function: function.clone(),
                            args: processed_args,
                        },
                        expr.span
                    ))
                } else {
                    Err(TypeError::UndefinedVariable(module.clone(), expr.span))
                }
            },
            ExprKind::Return(ref value) => {
                // Process the returned value
                let processed_value = self.infer_expr_type(value)?;
                let value_type = self.infer_type_from_expr(&processed_value)?;
                
                // Return expression has the same type as its value
                Ok(Located::new(
                    ExprKind::Return(Box::new(processed_value)),
                    expr.span
                ))
            },
            ExprKind::Quote(inner) => {
                // Quoted expressions are not evaluated, so we don't need to check their types
                Ok(expr.clone())
            },
            ExprKind::Unquote(inner) => {
                // Unquoted expressions should only appear inside quasi-quotes
                // This is a syntax error that should be caught by the parser
                // For type inference, we'll just pass it through
                Ok(expr.clone())
            },
            ExprKind::UnquoteSplicing(inner) => {
                // Unquote-splicing expressions should only appear inside quasi-quotes
                // This is a syntax error that should be caught by the parser
                // For type inference, we'll just pass it through
                Ok(expr.clone())
            },
            ExprKind::QuasiQuote(inner) => {
                // Quasi-quoted expressions are like quoted expressions
                // We don't need to check their types
                Ok(expr.clone())
            },
        }
    }
    
    /// Process a pattern in a match expression
    fn process_pattern(&mut self, pattern: &crate::ast::Pattern, expected_type: &Type) -> Result<crate::ast::Pattern, TypeError> {
        match pattern {
            crate::ast::Pattern::Literal(lit) => {
                let lit_type = self.infer_literal_type(lit);
                if lit_type != *expected_type {
                    // Allow some type flexibility for literals
                    match (lit_type.clone(), expected_type) {
                        // Allow integer literals in any integer context
                        (Type::Int(_), Type::Int(_)) => {},
                        (Type::Int(_), Type::UInt(_)) => {},
                        (Type::UInt(_), Type::Int(_)) => {},
                        (Type::UInt(_), Type::UInt(_)) => {},
                        
                        // Allow float literals in any float context
                        (Type::Float(_), Type::Float(_)) => {},
                        
                        // Other types must match exactly
                        _ => {
                            return Err(TypeError::TypeMismatch {
                                expected: expected_type.clone(),
                                found: lit_type.clone(),
                                span: Span::new(0, 0), // No span info for patterns
                            });
                        }
                    }
                }
                Ok(pattern.clone())
            },
            
            crate::ast::Pattern::Binding(name) => {
                // Register the binding in the symbol table
                self.symbol_table.add_variable(name, expected_type.clone());
                Ok(pattern.clone())
            },
            
            crate::ast::Pattern::Wildcard => {
                // Wildcard matches anything, no type checking needed
                Ok(pattern.clone())
            },
            
            crate::ast::Pattern::Constructor { tag, subpattern } => {
                // Find a matching data type
                let mut found_type = false;
                let mut expected_value_type = None;
                
                for (_, ty) in &self.symbol_table.variables {
                    if let Type::Data(variants) = ty {
                        if let Some((_, value_type_opt)) = variants.iter().find(|(variant_tag, _)| variant_tag == tag) {
                            found_type = true;
                            
                            // Check if the pattern structure matches
                            if subpattern.is_some() != value_type_opt.is_some() {
                                return Err(TypeError::InvalidDataValue {
                                    expected: value_type_opt.clone().unwrap_or(Type::Named("void".to_string())),
                                    found: Type::Named("pattern-mismatch".to_string()),
                                    span: Span::new(0, 0),
                                });
                            }
                            
                            expected_value_type = value_type_opt.clone();
                            break;
                        }
                    }
                }
                
                if !found_type {
                    return Err(TypeError::InvalidDataTag {
                        data_type: expected_type.clone(),
                        tag: tag.clone(),
                        span: Span::new(0, 0),
                    });
                }
                
                // Check subpattern if present
                let processed_subpattern = if let Some(subpat) = subpattern {
                    if let Some(value_type) = &expected_value_type {
                        Some(Box::new(self.process_pattern(subpat, value_type)?))
                    } else {
                        // No subpattern expected
                        return Err(TypeError::InvalidDataValue {
                            expected: Type::Named("void".to_string()),
                            found: Type::Named("pattern".to_string()),
                            span: Span::new(0, 0),
                        });
                    }
                } else {
                    None
                };
                
                Ok(crate::ast::Pattern::Constructor {
                    tag: tag.clone(),
                    subpattern: processed_subpattern,
                })
            },
            
            crate::ast::Pattern::List(subpatterns) => {
                // Ensure we're matching against a tuple or array type
                let (element_types, is_tuple) = match expected_type {
                    Type::Tuple(types) => (types.clone(), true),
                    Type::Array(ty, size_opt) => {
                        if let Some(size) = size_opt {
                            if *size != subpatterns.len() {
                                return Err(TypeError::TypeMismatch {
                                    expected: expected_type.clone(),
                                    found: Type::Tuple(vec![]), // Placeholder
                                    span: Span::new(0, 0),
                                });
                            }
                        }
                        
                        // Create a list of the same element type
                        let types = (0..subpatterns.len()).map(|_| *ty.clone()).collect();
                        (types, false)
                    },
                    _ => {
                        return Err(TypeError::TypeMismatch {
                            expected: expected_type.clone(),
                            found: Type::Tuple(vec![]), // Placeholder
                            span: Span::new(0, 0),
                        });
                    }
                };
                
                // Check that the pattern has the right number of elements
                if subpatterns.len() != element_types.len() && is_tuple {
                    return Err(TypeError::TypeMismatch {
                        expected: expected_type.clone(),
                        found: Type::Tuple(vec![]), // Placeholder
                        span: Span::new(0, 0),
                    });
                }
                
                // Process each subpattern
                let mut processed_subpatterns = Vec::new();
                for (i, subpat) in subpatterns.iter().enumerate() {
                    if i < element_types.len() {
                        let processed = self.process_pattern(subpat, &element_types[i])?;
                        processed_subpatterns.push(processed);
                    } else {
                        // This shouldn't happen due to the length check above
                        return Err(TypeError::TypeMismatch {
                            expected: expected_type.clone(),
                            found: Type::Tuple(vec![]), // Placeholder
                            span: Span::new(0, 0),
                        });
                    }
                }
                
                Ok(crate::ast::Pattern::List(processed_subpatterns))
            },
        }
    }
    
    /// Infer the type of an expression
    fn infer_type_from_expr(&self, expr: &Expr) -> Result<Type, TypeError> {
        match &expr.node {
            ExprKind::Literal(literal) => Ok(self.infer_literal_type(literal)),
            ExprKind::Symbol(name) => {
                if let Some(var_type) = self.symbol_table.get_variable_type(name) {
                    Ok(var_type.clone())
                } else {
                    Err(TypeError::UndefinedVariable(name.clone(), expr.span))
                }
            },
            ExprKind::Addr(inner) => {
                let inner_type = self.infer_type_from_expr(inner)?;
                Ok(Type::Pointer(Box::new(inner_type)))
            },
            ExprKind::Load(inner) => {
                let inner_type = self.infer_type_from_expr(inner)?;
                match inner_type {
                    Type::Pointer(pointed_to) => Ok(*pointed_to),
                    _ => Err(TypeError::DerefOfNonPointer(expr.span)),
                }
            },
            ExprKind::Store { addr, value } => {
                let addr_type = self.infer_type_from_expr(addr)?;
                let value_type = self.infer_type_from_expr(value)?;
                match addr_type {
                    Type::Pointer(pointed_to) if *pointed_to == value_type => Ok(Type::Named("void".to_string())),
                    Type::Pointer(pointed_to) => Err(TypeError::TypeMismatch { expected: *pointed_to, found: value_type, span: expr.span }),
                    _ => Err(TypeError::DerefOfNonPointer(expr.span)),
                }
            },
            ExprKind::FieldAccess { object, field } => {
                let object_type = self.infer_type_from_expr(object)?;
                
                // Resolve the object type if it's a named type
                let resolved_type = self.symbol_table.resolve_type(&object_type);
                
                match resolved_type {
                    Type::Struct(fields) => {
                        // Find the field in the struct
                        if let Some((_, field_type)) = fields.iter().find(|(name, _)| name == field) {
                            Ok(field_type.clone())
                        } else {
                            Err(TypeError::FieldNotFound {
                                struct_type: object_type,
                                field: field.clone(),
                                span: expr.span,
                            })
                        }
                    },
                    _ => Err(TypeError::NotAStruct {
                        ty: object_type,
                        span: expr.span,
                    }),
                }
            },
            ExprKind::SetField { object: _, field: _, value: _ } => {
                // Field set operations return void
                Ok(Type::Named("void".to_string()))
            },
            ExprKind::SetIndex { array: _, index: _, value: _ } => {
                // Array set operations return void
                Ok(Type::Named("void".to_string()))
            },
            ExprKind::SetAddr { addr: _, value: _ } => {
                // Address set operations return void
                Ok(Type::Named("void".to_string()))
            },
            ExprKind::Call { name, args } => {
                // Special case for type constructors
                if let Some(basic_type) = self.get_basic_type_from_name(name) {
                    if name == "ptr" && args.len() >= 1 {
                        // For ptr type constructor, get the pointed-to type
                        // For example, in (ptr i32 addr_expr), we need the i32 type
                        if let ExprKind::Symbol(type_name) = &args[0].node {
                            if let Some(pointed_type) = self.get_basic_type_from_name(type_name) {
                                return Ok(Type::Pointer(Box::new(pointed_type)));
                            } else if let Some(ty) = self.symbol_table.get_variable_type(type_name) {
                                return Ok(Type::Pointer(Box::new(ty.clone())));
                            }
                        }
                        // Default fallback
                        return Ok(Type::Pointer(Box::new(Type::Named("any".to_string()))));
                    }
                    
                    // For basic type constructors like (i32 42), return the basic type
                    return Ok(basic_type);
                }
                
                // Check if this is a named type constructor
                if let Some(ty) = self.symbol_table.get_variable_type(name) {
                    // For named type constructors like custom types, 
                    // return the type directly
                    return Ok(ty.clone());
                }
                
                // Special case for the "is" type check operator
                if name == "is" && args.len() == 2 {
                    // Type checking always returns a boolean
                    return Ok(Type::Bool);
                }
                
                // For regular function calls, assume a generic return type
                // In a more complete implementation, we would look up the function's signature
                Ok(Type::Named("any".to_string()))
            },
            ExprKind::Do(expressions) => {
                if let Some(last_expr) = expressions.last() {
                    self.infer_type_from_expr(last_expr)
                } else {
                    Ok(Type::Named("void".to_string()))
                }
            },
            ExprKind::If { then_branch, else_branch, .. } => {
                let then_type = self.infer_type_from_expr(then_branch)?;
                if let Some(else_expr) = else_branch {
                    let else_type = self.infer_type_from_expr(else_expr)?;
                    if then_type != else_type {
                        return Err(TypeError::TypeMismatch { expected: then_type, found: else_type, span: expr.span });
                    }
                }
                Ok(then_type)
            },
            ExprKind::For { .. } => {
                // For loops always return void
                Ok(Type::Named("void".to_string()))
            },
            ExprKind::Binary { op, .. } => {
                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => Ok(Type::Int(32)),
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => Ok(Type::Bool),
                }
            },
            ExprKind::Literal(Literal::Tuple(elements)) => {
                // Infer the type of each element in the tuple
                let mut element_types = Vec::new();
                for elem in elements {
                    let elem_type = self.infer_type_from_expr(elem)?;
                    element_types.push(elem_type);
                }
                Ok(Type::Tuple(element_types))
            },
            ExprKind::DataConstructor { tag, .. } => {
                // Find a data type that contains this tag
                for (_, ty) in &self.symbol_table.variables {
                    if let Type::Data(variants) = ty {
                        if variants.iter().any(|(variant_tag, _)| variant_tag == tag) {
                            return Ok(ty.clone());
                        }
                    }
                }
                
                // If we can't find a matching data type, return an error
                Err(TypeError::InvalidDataTag {
                    data_type: Type::Named("unknown".to_string()),
                    tag: tag.clone(),
                    span: expr.span,
                })
            },
            ExprKind::Match { scrutinee: _, cases } => {
                // The type of a match expression is the type of the result expressions
                // (all cases must have the same type)
                if let Some(case) = cases.first() {
                    self.infer_type_from_expr(&case.result)
                } else {
                    // Empty match (should never happen due to parser constraints)
                    Ok(Type::Named("void".to_string()))
                }
            },
            ExprKind::TypeCheck { .. } => {
                // Type check operations always return a boolean
                Ok(Type::Bool)
            },
            ExprKind::ModuleCall { module: _, function: _, args } => {
                // For now, we'll assume module calls return a generic "any" type
                // In a more advanced implementation, we would look up the function's return type from the module
                Ok(Type::Named("any".to_string()))
            },
            ExprKind::Return(ref value) => {
                // Return expression has the same type as its value
                self.infer_type_from_expr(value)
            },
            ExprKind::Quote(_) => {
                // Quoted expressions are not evaluated, so they don't have a runtime type
                Ok(Type::Named("quote".to_string()))
            },
            ExprKind::Unquote(_) => {
                // Unquote expressions should only appear inside quasi-quotes
                // This is a syntax error that should be caught by the parser
                Ok(Type::Named("unquote".to_string()))
            },
            ExprKind::UnquoteSplicing(_) => {
                // Unquote-splicing expressions should only appear inside quasi-quotes
                // This is a syntax error that should be caught by the parser
                Ok(Type::Named("unquote-splicing".to_string()))
            },
            ExprKind::QuasiQuote(_) => {
                // Quasi-quoted expressions are like quoted expressions
                Ok(Type::Named("quasi-quote".to_string()))
            },
        }
    }
    
    /// Infer the type of a literal
    fn infer_literal_type(&self, literal: &Literal) -> Type {
        match literal {
            Literal::Integer(_) => Type::Int(32), // Default to i32
            Literal::Float(_) => Type::Float(64), // Default to f64
            Literal::Boolean(_) => Type::Bool,
            Literal::Char(_) => Type::Int(8), // Char is i8
            Literal::String(_) => Type::Array(Box::new(Type::Int(8)), None), // String is [i8]
            Literal::Atom(_) => Type::Atom,
            Literal::Null => Type::Pointer(Box::new(Type::Named("void".to_string()))), // null is (ptr void)
            Literal::List(_) => Type::Array(Box::new(Type::Named("any".to_string())), None), // List is [any]
            Literal::Tuple(elements) => {
                // For a tuple literal, infer the type of each element
                let mut element_types = Vec::new();
                for elem in elements {
                    if let Ok(elem_type) = self.infer_type_from_expr(elem) {
                        element_types.push(elem_type);
                    } else {
                        // If we can't infer the type, use a placeholder
                        element_types.push(Type::Named("unknown".to_string()));
                    }
                }
                Type::Tuple(element_types)
            },
        }
    }
    
    // Check if a value of type `value_type` can be a member of `union_type`
    fn is_compatible_with_union(&self, value_type: &Type, union_type: &Type) -> bool {
        match union_type {
            Type::Union(types) => {
                // Check if the value type matches any of the union's member types
                types.iter().any(|t| self.types_are_compatible(value_type, t))
            },
            _ => false,
        }
    }
    
    // Check if two types are compatible (for type checking)
    fn types_are_compatible(&self, type1: &Type, type2: &Type) -> bool {
        match (type1, type2) {
            // Exact match
            (a, b) if a == b => true,
            
            // Named types - resolve and check
            (Type::Named(name), other) => {
                if let Some(resolved) = self.symbol_table.type_aliases.get(name) {
                    self.types_are_compatible(resolved, other)
                } else {
                    false
                }
            },
            (other, Type::Named(name)) => {
                if let Some(resolved) = self.symbol_table.type_aliases.get(name) {
                    self.types_are_compatible(other, resolved)
                } else {
                    false
                }
            },
            
            // Union types - check if type1 is compatible with any member of the union
            (value_type, Type::Union(union_types)) => {
                union_types.iter().any(|ut| self.types_are_compatible(value_type, ut))
            },
            
            // Pointers - check if the pointed types are compatible
            (Type::Pointer(t1), Type::Pointer(t2)) => self.types_are_compatible(t1, t2),
            
            // Arrays - check if the element types are compatible
            (Type::Array(t1, s1), Type::Array(t2, s2)) => {
                let sizes_match = match (s1, s2) {
                    (None, _) | (_, None) => true, // Dynamic size arrays match any size
                    (Some(size1), Some(size2)) => size1 == size2,
                };
                
                sizes_match && self.types_are_compatible(t1, t2)
            },
            
            // Tuples - check if all element types are compatible
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                types1.len() == types2.len() && 
                types1.iter().zip(types2.iter()).all(|(t1, t2)| self.types_are_compatible(t1, t2))
            },
            
            // Structs - check if all field types are compatible
            (Type::Struct(fields1), Type::Struct(fields2)) => {
                fields1.len() == fields2.len() && 
                fields1.iter().all(|(name1, ty1)| {
                    fields2.iter().any(|(name2, ty2)| {
                        name1 == name2 && self.types_are_compatible(ty1, ty2)
                    })
                })
            },
            
            // Data types - check if the variants match
            (Type::Data(variants1), Type::Data(variants2)) => {
                variants1.len() == variants2.len() && 
                variants1.iter().all(|(tag1, ty_opt1)| {
                    variants2.iter().any(|(tag2, ty_opt2)| {
                        tag1 == tag2 && match (ty_opt1, ty_opt2) {
                            (Some(ty1), Some(ty2)) => self.types_are_compatible(ty1, ty2),
                            (None, None) => true,
                            _ => false,
                        }
                    })
                })
            },
            
            // Function types - check if parameter and return types are compatible
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                params1.len() == params2.len() && 
                self.types_are_compatible(ret1, ret2) && 
                params1.iter().zip(params2.iter()).all(|(p1, p2)| self.types_are_compatible(p1, p2))
            },
            
            // All other combinations are not compatible
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;
    
    #[test]
    fn test_infer_atom_type() {
        let src = r#"
        (type status atom)
        (def success :ok)
        (def failure :error)
        (def custom-status :my-value)
        "#;
        
        let program = parser::parse_program(src).unwrap();
        let mut inferer = TypeInferer::new();
        let result = inferer.process_program(&program).unwrap();
        
        // Check that atoms are correctly typed
        if let TopLevelKind::VarDef { name, value } = &result.forms[1].node {
            assert_eq!(name, "success");
            if let ExprKind::Literal(Literal::Atom(atom_name)) = &value.node {
                assert_eq!(atom_name, "ok");
            } else {
                panic!("Expected Atom literal");
            }
        }
        
        // Check atom with a custom status type
        if let TopLevelKind::VarDef { name, value } = &result.forms[3].node {
            assert_eq!(name, "custom-status");
            if let ExprKind::Literal(Literal::Atom(atom_name)) = &value.node {
                assert_eq!(atom_name, "my-value");
            } else {
                panic!("Expected Atom literal");
            }
        }
    }
    
    #[test]
    fn test_array_with_new_syntax() {
        let src = r#"
        (type int-array [i32, 5])
        (def numbers (int-array))
        "#;
        
        let program = parser::parse_program(src).unwrap();
        let mut inferer = TypeInferer::new();
        let result = inferer.process_program(&program);
        
        // Check that the program processed successfully
        assert!(result.is_ok(), "Failed to process program: {:?}", result.err());
        
        // No need to check the specific value types, just make sure it doesn't error
    }
    
    #[test]
    fn test_tuple_type_inference() {
        let src = r#"
        (def a [20 true null])
        (def b [1 2 3])
        (def c ["hello" :world])
        "#;
        
        let program = parser::parse_program(src).unwrap();
        let mut inferer = TypeInferer::new();
        let result = inferer.process_program(&program).unwrap();
        
        // Check that tuple a has the correct types: [i32 bool (ptr void)]
        if let TopLevelKind::VarDef { name, value: _ } = &result.forms[0].node {
            assert_eq!(name, "a");
            let var_type = inferer.symbol_table.get_variable_type("a").unwrap();
            if let Type::Tuple(types) = var_type {
                assert_eq!(types.len(), 3);
                assert_eq!(types[0], Type::Int(32));
                assert_eq!(types[1], Type::Bool);
                assert_eq!(types[2], Type::Pointer(Box::new(Type::Named("void".to_string()))));
            } else {
                panic!("Expected Tuple type, got {:?}", var_type);
            }
        }
        
        // Check that tuple b has the correct types: [i32 i32 i32]
        if let TopLevelKind::VarDef { name, value: _ } = &result.forms[1].node {
            assert_eq!(name, "b");
            let var_type = inferer.symbol_table.get_variable_type("b").unwrap();
            if let Type::Tuple(types) = var_type {
                assert_eq!(types.len(), 3);
                assert_eq!(types[0], Type::Int(32));
                assert_eq!(types[1], Type::Int(32));
                assert_eq!(types[2], Type::Int(32));
            } else {
                panic!("Expected Tuple type, got {:?}", var_type);
            }
        }
        
        // Check that tuple c has the correct types: [[i8] atom]
        if let TopLevelKind::VarDef { name, value: _ } = &result.forms[2].node {
            assert_eq!(name, "c");
            let var_type = inferer.symbol_table.get_variable_type("c").unwrap();
            if let Type::Tuple(types) = var_type {
                assert_eq!(types.len(), 2);
                assert_eq!(types[0], Type::Array(Box::new(Type::Int(8)), None));
                assert_eq!(types[1], Type::Atom);
            } else {
                panic!("Expected Tuple type, got {:?}", var_type);
            }
        }
    }

    #[test]
    fn test_union_type_inference() {
        let src = r#"
        (type number (union u8 f32))
        (def x (number 42))
        (def y (number 3.14))
        (def x-is-int (is u8 x))
        (def y-is-float (is f32 y))
        "#;
        
        let program = parser::parse_program(src).unwrap();
        let mut inferer = TypeInferer::new();
        let result = inferer.process_program(&program);
        
        // Check that the program processed successfully
        assert!(result.is_ok(), "Failed to process program: {:?}", result.err());
        
        // Check that x is inferred as the union type
        let x_type = inferer.symbol_table.get_variable_type("x").unwrap();
        if let Type::Union(types) = inferer.symbol_table.resolve_type(x_type) {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&Type::UInt(8)));
            assert!(types.contains(&Type::Float(32)));
        } else {
            panic!("Expected Union type for x, got {:?}", x_type);
        }
        
        // Check that type checking expressions return bool
        let x_is_int_type = inferer.symbol_table.get_variable_type("x-is-int").unwrap();
        assert_eq!(*x_is_int_type, Type::Bool);
        
        let y_is_float_type = inferer.symbol_table.get_variable_type("y-is-float").unwrap();
        assert_eq!(*y_is_float_type, Type::Bool);
    }
} 