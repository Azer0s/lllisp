#[cfg(test)]
mod tests {
    use lllisp::parser::parse_program;
    use lllisp::type_inference::TypeInferer;

    #[test]
    fn test_header_nested_path_validation() {
        // This should fail because stdio is a header module and cannot have nested paths
        let invalid_src = r#"
        (def stdio (use :header "stdio.h"))
        (stdio/nested/printf "Hello, world")
        "#;
        
        let program = parse_program(invalid_src).unwrap();
        let mut inferer = TypeInferer::new();
        let result = inferer.process_program(&program);
        
        // The type inference should fail because header modules cannot have nested paths
        assert!(result.is_err(), "Should fail with invalid module path for header import");
        
        // This should work because it's a direct function call on the header
        let valid_src = r#"
        (def stdio (use :header "stdio.h"))
        (stdio/printf "Hello, world")
        "#;
        
        let program = parse_program(valid_src).unwrap();
        let mut inferer = TypeInferer::new();
        let result = inferer.process_program(&program);
        
        // The type inference should succeed because the call is valid
        assert!(result.is_ok(), "Should succeed with direct function call on header import");
        
        // This should work because regular modules can have nested paths
        let valid_nested_src = r#"
        (def graphics (use "graphics.ll"))
        (graphics/ui/button/render "Click Me")
        "#;
        
        let program = parse_program(valid_nested_src).unwrap();
        let mut inferer = TypeInferer::new();
        let result = inferer.process_program(&program);
        
        // The type inference should succeed because regular modules can have nested paths
        assert!(result.is_ok(), "Should succeed with nested path for regular module import");
    }
} 