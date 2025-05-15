/// External Function Mapping for LLLisp
///
/// This module provides functionality to map external functions from C header files
/// using libclang. It extracts function signatures and makes them available to the compiler.

use std::collections::HashMap;
use clang::{Clang, Entity, EntityKind, Index, TranslationUnit, Type as ClangType};
use crate::ast::{Type, Program, TopLevelKind};

/// Represents an external function
#[derive(Debug, Clone)]
pub struct ExternalFunction {
    /// Name of the function
    pub name: String,
    /// Return type of the function
    pub return_type: Type,
    /// Parameter types of the function
    pub param_types: Vec<Type>,
    /// Whether the function takes a variable number of arguments
    pub is_variadic: bool,
}

/// Manages external function mappings
pub struct ExternalFunctionMapper {
    /// Map of module names to their function definitions
    modules: HashMap<String, HashMap<String, ExternalFunction>>,
}

impl ExternalFunctionMapper {
    /// Create a new external function mapper
    pub fn new() -> Result<Self, String> {
        // Ensure libclang is loaded
        if clang_sys::get_library().is_none() {
            return Err("Failed to load libclang library".to_string());
        }
        
        // Check if clang is available
        if let Err(e) = Clang::new() {
            return Err(format!("Failed to initialize libclang: {:?}", e));
        }
        
        Ok(Self {
            modules: HashMap::new(),
        })
    }
    
    /// Process a program to find and map external functions
    pub fn process(&mut self, program: &Program) -> Result<(), String> {
        // Find all module imports with headers
        for form in &program.forms {
            if let TopLevelKind::ModuleImport { name, path, is_header } = &form.node {
                if *is_header {
                    // Parse the header file and extract function definitions
                    self.parse_header(name, path)?;
                }
            }
        }
        
        Ok(())
    }
    
    /// Parse a header file and extract function definitions
    fn parse_header(&mut self, module_name: &str, header_path: &str) -> Result<(), String> {
        println!("Parsing header file: {}", header_path);
        
        // Initialize clang for this operation
        let clang = Clang::new().map_err(|e| format!("Failed to initialize libclang: {:?}", e))?;
        let index = Index::new(&clang, false, true);
        
        // Parse the translation unit
        let tu = match TranslationUnit::from_ast(
            &index,
            header_path,
        ) {
            Ok(tu) => tu,
            Err(e) => return Err(format!("Failed to parse header file {}: {:?}", header_path, e)),
        };
        
        // Extract function declarations
        let mut functions = HashMap::new();
        self.extract_functions(&tu.get_entity(), &mut functions)?;
        
        // Store the functions for this module
        self.modules.insert(module_name.to_string(), functions);
        
        Ok(())
    }
    
    /// Extract function declarations from a translation unit
    fn extract_functions(
        &self,
        entity: &Entity,
        functions: &mut HashMap<String, ExternalFunction>,
    ) -> Result<(), String> {
        // Process this entity if it's a function declaration
        if entity.get_kind() == EntityKind::FunctionDecl {
            let name = entity.get_name().unwrap_or_default();
            if !name.is_empty() {
                // Get function type
                let fn_type = entity.get_type().ok_or_else(|| {
                    format!("Failed to get type for function: {}", name)
                })?;
                
                // Get return type
                let return_type = fn_type.get_result_type().ok_or_else(|| {
                    format!("Failed to get return type for function: {}", name)
                })?;
                
                // Convert return type to LLLisp type
                let return_type = self.convert_clang_type(&return_type)?;
                
                // Get parameter types
                let mut param_types = Vec::new();
                
                // Use get_argument_types instead of get_num_arg_types/get_arg_type
                if let Some(arg_types) = fn_type.get_argument_types() {
                    for arg_type in arg_types {
                        let param_type = self.convert_clang_type(&arg_type)?;
                        param_types.push(param_type);
                    }
                }
                
                // Check if the function is variadic
                let is_variadic = fn_type.is_variadic();
                
                // Create external function
                let function = ExternalFunction {
                    name: name.clone(),
                    return_type,
                    param_types,
                    is_variadic,
                };
                
                // Add to map
                functions.insert(name, function);
            }
        }
        
        // Recursively process children
        for child in entity.get_children() {
            self.extract_functions(&child, functions)?;
        }
        
        Ok(())
    }
    
    /// Convert a Clang type to a LLLisp type
    fn convert_clang_type(&self, clang_type: &ClangType) -> Result<Type, String> {
        match clang_type.get_kind() {
            clang::TypeKind::Void => Ok(Type::Named("void".to_string())),
            
            clang::TypeKind::Bool => Ok(Type::Bool),
            
            clang::TypeKind::CharS | clang::TypeKind::SChar => Ok(Type::Int(8)),
            clang::TypeKind::UChar => Ok(Type::UInt(8)),
            
            clang::TypeKind::Short => Ok(Type::Int(16)),
            clang::TypeKind::UShort => Ok(Type::UInt(16)),
            
            clang::TypeKind::Int => Ok(Type::Int(32)),
            clang::TypeKind::UInt => Ok(Type::UInt(32)),
            
            clang::TypeKind::Long => Ok(Type::Int(64)),
            clang::TypeKind::ULong => Ok(Type::UInt(64)),
            
            clang::TypeKind::LongLong => Ok(Type::Int(64)),
            clang::TypeKind::ULongLong => Ok(Type::UInt(64)),
            
            clang::TypeKind::Float => Ok(Type::Float(32)),
            clang::TypeKind::Double => Ok(Type::Float(64)),
            clang::TypeKind::LongDouble => Ok(Type::Float(128)),
            
            clang::TypeKind::Pointer => {
                // Get the pointee type
                let pointee_type = clang_type.get_pointee_type().ok_or_else(|| {
                    "Failed to get pointee type".to_string()
                })?;
                
                // Handle char* as string
                if pointee_type.get_kind() == clang::TypeKind::CharS {
                    // For char*, we'll use a special string type
                    return Ok(Type::Named("string".to_string()));
                }
                
                // Convert the pointee type
                let pointee_lllisp_type = self.convert_clang_type(&pointee_type)?;
                
                Ok(Type::Pointer(Box::new(pointee_lllisp_type)))
            },
            
            clang::TypeKind::ConstantArray => {
                // Get the element type
                let element_type = clang_type.get_element_type().ok_or_else(|| {
                    "Failed to get array element type".to_string()
                })?;
                
                // Get the array size
                let size = clang_type.get_size().ok_or_else(|| {
                    "Failed to get array size".to_string()
                })?;
                
                // Convert the element type
                let element_lllisp_type = self.convert_clang_type(&element_type)?;
                
                Ok(Type::Array(Box::new(element_lllisp_type), Some(size as usize)))
            },
            
            clang::TypeKind::FunctionPrototype => {
                // Get return type
                let return_type = clang_type.get_result_type().ok_or_else(|| {
                    "Failed to get function return type".to_string()
                })?;
                
                // Convert return type
                let return_lllisp_type = self.convert_clang_type(&return_type)?;
                
                // Get parameter types
                let mut param_types = Vec::new();
                
                // Use get_argument_types instead of get_num_arg_types/get_arg_type
                if let Some(arg_types) = clang_type.get_argument_types() {
                    for arg_type in arg_types {
                        let param_type = self.convert_clang_type(&arg_type)?;
                        param_types.push(param_type);
                    }
                }
                
                Ok(Type::Function(param_types, Box::new(return_lllisp_type)))
            },
            
            clang::TypeKind::Record => {
                // For structs and unions, just use their name
                let type_name = clang_type.get_display_name();
                Ok(Type::Named(type_name))
            },
            
            clang::TypeKind::Enum => {
                // Treat enums as integers
                Ok(Type::Int(32))
            },
            
            clang::TypeKind::Typedef => {
                // For typedefs, get the canonical type
                let canonical_type = clang_type.get_canonical_type();
                self.convert_clang_type(&canonical_type)
            },
            
            _ => Err(format!("Unsupported type: {:?}", clang_type.get_kind())),
        }
    }
    
    /// Get a function definition for a given module and function name
    pub fn get_function(&self, module_name: &str, function_name: &str) -> Option<&ExternalFunction> {
        self.modules.get(module_name)?.get(function_name)
    }
    
    /// Check if a module has been loaded
    pub fn has_module(&self, module_name: &str) -> bool {
        self.modules.contains_key(module_name)
    }
    
    /// Get all functions for a module
    pub fn get_module_functions(&self, module_name: &str) -> Option<&HashMap<String, ExternalFunction>> {
        self.modules.get(module_name)
    }
}

/// Compiler pass for external function mapping
pub struct ExternalFunctionPass {
    mapper: ExternalFunctionMapper,
}

impl ExternalFunctionPass {
    /// Create a new external function pass
    pub fn new() -> Result<Self, String> {
        Ok(Self {
            mapper: ExternalFunctionMapper::new()?,
        })
    }
    
    /// Process a program to map external functions
    pub fn process(&mut self, program: &Program) -> Result<Program, String> {
        // Process the program to find and map external functions
        self.mapper.process(program)?;
        
        // Return the program unchanged
        // (This pass only collects information, it doesn't transform the AST)
        Ok(program.clone())
    }
    
    /// Get the external function mapper
    pub fn mapper(&self) -> &ExternalFunctionMapper {
        &self.mapper
    }
} 