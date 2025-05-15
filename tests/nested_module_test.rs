use lllisp::ast::{ExprKind, TopLevelKind};
use lllisp::parser::parse_program;

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_nested_module_paths() {
        let src = r#"
        (def graphics (use "graphics.ll"))
        (def utils (use "utils.ll"))
        
        (graphics/ui/button/render "Click Me" 100 200 150 50)
        (utils/string/format/pad-left "Hello" 10 " ")
        "#;
        
        let program = parse_program(src).unwrap();
        println!("Parsed forms: {}", program.forms.len());
        
        // We should have at least 4 forms
        assert!(program.forms.len() >= 4, "Expected at least 4 forms, got {}", program.forms.len());
        
        // Check graphics import
        if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[0].node {
            assert_eq!(name, "graphics");
            assert_eq!(path, "graphics.ll");
            assert!(!is_header);
        } else {
            panic!("Expected ModuleImport, got {:?}", program.forms[0].node);
        }
        
        // Check utils import
        if let TopLevelKind::ModuleImport { name, path, is_header } = &program.forms[1].node {
            assert_eq!(name, "utils");
            assert_eq!(path, "utils.ll");
            assert!(!is_header);
        } else {
            panic!("Expected ModuleImport, got {:?}", program.forms[1].node);
        }
        
        // Check graphics/ui/button/render module call
        if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[2].node {
            assert_eq!(module, "graphics/ui/button");
            assert_eq!(function, "render");
            assert_eq!(args.len(), 5);
        } else {
            panic!("Expected ModuleCall, got {:?}", program.forms[2].node);
        }
        
        // Check utils/string/format/pad-left module call
        if let TopLevelKind::Expr(ExprKind::ModuleCall { module, function, args }) = &program.forms[3].node {
            assert_eq!(module, "utils/string/format");
            assert_eq!(function, "pad-left");
            assert_eq!(args.len(), 3);
        } else {
            panic!("Expected ModuleCall, got {:?}", program.forms[3].node);
        }
    }
} 