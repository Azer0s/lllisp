/// Abstract Syntax Tree for LLLisp
use std::fmt;

/// Represents a position in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

/// Represents a node with source location
#[derive(Debug, Clone, PartialEq)]
pub struct Located<T> {
    pub span: Span,
    pub node: T,
}

impl<T> Located<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

/// Type expressions in the language
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Basic types
    Int(usize), // i8, i16, i32, i64, i128 with size in bits
    UInt(usize), // u8, u16, u32, u64, u128 with size in bits
    Float(usize), // f16, f32, f64, f128 with size in bits
    Bool,
    Atom, // New: Atom type for symbolic constants
    
    // Composite types
    Pointer(Box<Type>), // (ptr type)
    Array(Box<Type>, Option<usize>), // [type] or [type, size]
    Function(Vec<Type>, Box<Type>), // (fn [param-types...] return-type)
    
    // Struct type
    Struct(Vec<(String, Type)>), // (struct (:field1 type1) (:field2 type2) ...)
    
    // Generic struct type
    GenericStruct {
        type_params: Vec<String>,
        fields: Vec<(String, Type)>,
    }, // (struct<t1 t2...> (:field1 type1) (:field2 type2) ...)
    
    // Tuple type
    Tuple(Vec<Type>), // [type1 type2 ...]
    
    // Generic tuple type
    GenericTuple {
        type_params: Vec<String>,
        types: Vec<Type>,
    }, // (tuple<t1 t2...> type1 type2 ...)
    
    // Data type (tagged union / enum)
    Data(Vec<(String, Option<Type>)>), // (data [:tag1 type1] [:tag2] ...)
    
    // Generic data type
    GenericData {
        type_params: Vec<String>,
        variants: Vec<(String, Option<Type>)>,
    }, // (data<t1 t2...> [:tag1 type1] [:tag2] ...)
    
    // Union type
    Union(Vec<Type>), // (union type1 type2 ...)
    
    // Named/Custom type
    Named(String),
    
    // Generic type instantiation
    GenericInstance {
        base: String,
        type_args: Vec<Type>,
    }, // base<type_args...>
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int(bits) => write!(f, "i{}", bits),
            Type::UInt(bits) => write!(f, "u{}", bits),
            Type::Float(bits) => write!(f, "f{}", bits),
            Type::Bool => write!(f, "bool"),
            Type::Atom => write!(f, "atom"),
            Type::Pointer(ty) => write!(f, "(ptr {})", ty),
            Type::Array(ty, None) => write!(f, "[{}]", ty),
            Type::Array(ty, Some(size)) => write!(f, "[{}, {}]", ty, size),
            Type::Function(param_types, return_type) => {
                write!(f, "(fn [")?;
                for (i, param_type) in param_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", param_type)?;
                }
                write!(f, "] {})", return_type)
            },
            Type::Struct(fields) => {
                write!(f, "(struct ")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "(:{} {})", name, ty)?;
                }
                write!(f, ")")
            },
            Type::GenericStruct { type_params, fields } => {
                write!(f, "(struct<")?;
                for (i, param) in type_params.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, "> ")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "(:{} {})", name, ty)?;
                }
                write!(f, ")")
            },
            Type::Tuple(types) => {
                write!(f, "[")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, "]")
            },
            Type::GenericTuple { type_params, types } => {
                write!(f, "(tuple<")?;
                for (i, param) in type_params.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, "> ")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            },
            Type::Data(variants) => {
                write!(f, "(data ")?;
                for (i, (tag, ty_opt)) in variants.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "[:{}", tag)?;
                    if let Some(ty) = ty_opt {
                        write!(f, " {}", ty)?;
                    }
                    write!(f, "]")?;
                }
                write!(f, ")")
            },
            Type::GenericData { type_params, variants } => {
                write!(f, "(data<")?;
                for (i, param) in type_params.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, "> ")?;
                for (i, (tag, ty_opt)) in variants.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "[:{}", tag)?;
                    if let Some(ty) = ty_opt {
                        write!(f, " {}", ty)?;
                    }
                    write!(f, "]")?;
                }
                write!(f, ")")
            },
            Type::Union(types) => {
                write!(f, "(union ")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            },
            Type::Named(name) => write!(f, "{}", name),
            Type::GenericInstance { base, type_args } => {
                write!(f, "{}<", base)?;
                for (i, ty) in type_args.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ">")
            },
        }
    }
}

/// Expression literals
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i128),
    Float(f64),
    Boolean(bool),
    Char(u8),
    String(String),
    Atom(String), // Atom literal with name
    List(Vec<Expr>), // List of expressions
    Tuple(Vec<Expr>), // Tuple of expressions
    Null, // Null literal (null pointer)
}

/// Expression types
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    // Literal values
    Literal(Literal),
    
    // Variables
    Symbol(String),
    
    // Memory operations
    Addr(Box<Expr>), // (addr expr)
    Load(Box<Expr>), // (load expr)
    Store {
        addr: Box<Expr>,
        value: Box<Expr>,
    }, // (store addr value)
    
    // Struct field access
    FieldAccess {
        object: Box<Expr>,
        field: String,
    }, // ($ :field object)
    
    // Array/struct/memory set
    SetField {
        object: Box<Expr>,
        field: String,
        value: Box<Expr>,
    }, // ($ :field object value)
    
    SetIndex {
        array: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
    }, // ($ [index] array value)
    
    SetAddr {
        addr: Box<Expr>,
        value: Box<Expr>,
    }, // ($ addr value)
    
    // Data variant constructor
    DataConstructor {
        tag: String,
        value: Option<Box<Expr>>,
    }, // [:tag value?]
    
    // Function call
    Call {
        name: String,
        args: Vec<Expr>,
    }, // (name arg1 arg2 ...)
    
    // Module function call
    ModuleCall {
        module: String,     // Full module path (can include multiple segments with /)
        function: String,
        args: Vec<Expr>,
    }, // (module[/submodule]/function arg1 arg2 ...)
    
    // Block of expressions
    Do(Vec<Expr>), // (do expr1 expr2 ...)
    
    // Return expression (added for compiler pass)
    Return(Box<Expr>), // Explicit return expression
    
    // Conditional expression
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>, // Optional else branch
    }, // (if condition then-expr else-expr?)
    
    // For loop expressions
    For {
        iterator: Option<Box<ForIterator>>, // Optional iterator (none means infinite loop)
        body: Box<Expr>,                   // Loop body
    }, // (for iterator body) or (for condition body)
    
    // Pattern matching
    Match {
        scrutinee: Box<Expr>,
        cases: Vec<MatchCase>,
    }, // (match expr [pattern1] result1 [pattern2] result2 ...)
    
    // Binary operations
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    }, // (op left right)
    
    // Type check operation
    TypeCheck {
        value: Box<Expr>,
        check_type: Type,
    }, // (is type value)
    
    // Quoting and unquoting for macros
    Quote(Box<Located<ExprKind>>),
    Unquote(Box<Located<ExprKind>>),
    UnquoteSplicing(Box<Located<ExprKind>>),
    QuasiQuote(Box<Located<ExprKind>>),
}

/// Loop iterator types
#[derive(Debug, Clone, PartialEq)]
pub enum ForIterator {
    // Condition-based loop (while)
    Condition(Expr),
    
    // Range-based loop: (range var collection)
    Range {
        var: String,
        collection: Expr,
    },
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    // Arithmetic
    Add,      // +
    Sub,      // -
    Mul,      // *
    Div,      // /
    Mod,      // %
    
    // Comparison
    Eq,       // ==
    Ne,       // !=
    Lt,       // <
    Gt,       // >
    Le,       // <=
    Ge,       // >=
}

/// Pattern for pattern matching
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    // Literal pattern (exact match)
    Literal(Literal),
    
    // Variable binding pattern (always matches and binds the value)
    Binding(String),
    
    // Wildcard pattern (matches anything without binding)
    Wildcard,
    
    // Constructor pattern [:tag] or [:tag pattern]
    Constructor {
        tag: String,
        subpattern: Option<Box<Pattern>>,
    },
    
    // List pattern that matches a sequence [pattern1 pattern2 ...]
    List(Vec<Pattern>),
}

/// A match case consists of a pattern and the result expression
#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub result: Expr,
}

pub type Expr = Located<ExprKind>;

/// Top-level declarations
#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelKind {
    // Type definition
    TypeDef {
        name: String,
        ty: Type,
    }, // (type name type-expr)
    
    // Variable definition
    VarDef {
        name: String,
        value: Located<ExprKind>,
    }, // (def name value)
    
    // Module import
    ModuleImport {
        name: String,
        path: String,
        is_header: bool,
    }, // (def name (use "path")) or (def name (use :header "path"))
    
    // Function alias definition
    Alias {
        name: String,
        module: String,
        function: String,
    }, // (alias name module/function)
    
    // Expression
    Expr(ExprKind),
    
    // Macro definition
    MacroDef {
        name: String,
        params: Vec<String>,
        body: Located<ExprKind>,
    }, // (macro name (params...) body)
}

pub type TopLevel = Located<TopLevelKind>;

/// A complete program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub forms: Vec<TopLevel>,
} 
