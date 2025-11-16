use lexer_project::lexer_regex::Lexer;
use lexer_project::parser_regex::Parser;
use lexer_project::scope_analyzer::ScopeAnalyzer;
use lexer_project::TypeChecker;

fn main() {
    let src = r#"
        fn int add(int x, int y) {
            int z = x;
            return z;
        }
        int a = 5;
    "#;

    // lexer + parser
    let lexer = Lexer::new();
    let tokens = lexer.tokenize(src);
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program().expect("parse error");

    // scope analysis (populate symbol table)
    let mut sa = ScopeAnalyzer::new();
    sa.analyze(&ast);

    // type checking reusing scope analyzer
    let mut tc = TypeChecker::new(&mut sa); // <-- use new() instead of from_scope_analyzer
    let has_errors = tc.check_program(&ast);

    if has_errors {
        println!("Type check errors:");
        for e in tc.errors {
            println!("{:?}", e);
        }
    } else {
        println!("No type errors ");
    }
}
