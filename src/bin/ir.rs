use lexer_project::lexer_regex::Lexer;
use lexer_project::parser_regex::Parser;
use lexer_project::scope_analyzer::ScopeAnalyzer;
use lexer_project::ir_gen::IRGenerator;

fn main() {
    let src = r#"
        fn int add(int x, int y) {
            int z = x + y;
            return z;
        }

        int a = 5;
        int b = add(a, 10);
    "#;

    let lexer = Lexer::new();
    let tokens = lexer.tokenize(src);

    let mut parser = Parser::new(tokens);
    let prog = parser.parse_program().expect("Parse error");

    let mut sa = ScopeAnalyzer::new();
    sa.analyze(&prog);

    let mut irgen = IRGenerator::new();
    let ir = irgen.generate(&prog);

    println!("===== IR OUTPUT =====");
    for func in &ir {
        println!("Function {}:", func.name);
        for inst in &func.instrs {
            println!("  {}", inst);
        }
        println!();
    }
}

