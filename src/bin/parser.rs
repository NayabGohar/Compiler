use lexer_project::lexer_regex::Lexer;
use lexer_project::parser_regex::{write_bnf_file, run_and_print};


///MAIN



fn main() {
    write_bnf_file();

    let sample = r#"
        fn int add(int x, int y) {
            int z = x;
            return z;
        }
        int a = 5;
    "#;

    println!("TEST: sample");
    run_and_print(sample);

    let exprs = r#"
        fn int main() {
            int x = 1 + 2 * 3 - 5234 / div * 3 + -h;


        }
    "#;

    println!("\nTEST: expressions & if/return");
    run_and_print(exprs);

    let more = r#"
        fn int calltest() {
            int r = sum(3, 4 * 5);
            return r;
        }
    "#;

    println!("\nTEST: calls");
    run_and_print(more);

    println!("\nMade 'language_bnf.md'.");
}
