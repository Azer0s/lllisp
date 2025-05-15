use crate::ast::{Expr, ExprKind, Literal, Span, Located};

/// Create a range object from 0 to max
pub fn to_range(max: i128) -> Vec<i128> {
    (0..max).collect()
}

/// Create a list from a vector of elements
pub fn create_list(elements: Vec<Expr>) -> Expr {
    Located::new(
        ExprKind::Literal(Literal::List(elements)),
        Span::new(0, 0)
    )
}

/// Register builtin functions for use at runtime
pub fn register_builtins() -> Vec<(&'static str, fn() -> Expr)> {
    vec![
        ("to", || {
            Located::new(
                ExprKind::Call {
                    name: "range_creator".to_string(),
                    args: Vec::new(),
                },
                Span::new(0, 0)
            )
        }),
        
        ("list", || {
            Located::new(
                ExprKind::Call {
                    name: "list_creator".to_string(),
                    args: Vec::new(),
                },
                Span::new(0, 0)
            )
        }),
        
        ("out", || {
            Located::new(
                ExprKind::Call {
                    name: "print".to_string(),
                    args: Vec::new(),
                },
                Span::new(0, 0)
            )
        }),
    ]
} 