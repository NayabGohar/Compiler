mod lexer_regex;
mod lexer_manual;

fn main() {
    let code = r#"
        fn int my_fn(int x, float y) {
            string my_str = "hello";
            bool my_bool = x == 40;
            return x;
        }
    "#;

    println!("--- Regex Lexer ---");
    let tokens_regex = lexer_regex::lexer_regex(code);
    for t in tokens_regex {
        println!("{:?}", t);
    }

    println!("\n--- Manual Lexer ---");
    let tokens_manual = lexer_manual::lexer_manual(code);
    for t in tokens_manual {
        println!("{:?}", t);
    }
}
