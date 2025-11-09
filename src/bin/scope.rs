use lexer_project::lexer_regex::Lexer;
use lexer_project::parser_regex::Parser;
use lexer_project::scope_analyzer::ScopeAnalyzer;

fn main() {

    // Example 1: No scope errors
    let valid_source = r#"
        fn int add(int x, int y) {
            int z = x + y;
            return z;
        }
        int a = 5;
    "#;

    println!("Example 1: Valid Program (No Errors)");
    run_scope_analysis(valid_source);

    // Example 2: with scope errors
    let invalid_source = r#"
        fn int add(int x, int y) {
            int z = x;
            return z;
        }
        int a = 5;

        fn int add(int x, int y) {
            int z = x + b; // b undeclared
            return z;
        }
    "#;

    println!("\nExample 2: Invalid Program (With Errors)");
    run_scope_analysis(invalid_source);

}

fn run_scope_analysis(source: &str) {
    let lexer = Lexer::new();
    let tokens = lexer.tokenize(source);

    let mut parser = Parser::new(tokens);
    let ast = match parser.parse_program() {
        Ok(ast) => ast,
        Err(e) => {
            println!("Parser error: {:?}", e);
            return;
        }
    };

    let mut analyzer = ScopeAnalyzer::new();
    analyzer.analyze(&ast);

    println!(" Result:");

    if analyzer.errors.is_empty() {
        println!(" No scope errors detected!");
    } else {
        println!(" Scope errors found ({}):", analyzer.errors.len());
        for (i, e) in analyzer.errors.iter().enumerate() {
            println!("  {}. {:?}", i + 1, e);
        }
    }

}
