use lexer_project::lexer_regex::Lexer;


fn run_test_case(name: &str, input: &str, lexer: &Lexer) {
    println!("=== TEST: {} ===", name);
    let tokens = lexer.tokenize(input);
    println!("{:?}\n", tokens);
}

fn main() {
    let lexer = Lexer::new();

    run_test_case("Sample program (from prompt)", r#"
        fn int my_fn(int x, float y) {
            string my_str = "hmm";
            bool my_bool = x == 40;
            return x;
        }
    "#, &lexer);

    run_test_case("Spaces & tabs & newlines", "fn int a ( ) ;", &lexer);

    run_test_case("Comments", r#"
// single line
fn main() {
    /* multiline
       comment */
    return 0;
}
    "#, &lexer);

    run_test_case("Operators & bitwise & shifts", r#"
        a = b & c | d ^ ~ e;
        x = y * 2;
        z = w * 1;
        if (a <= b && c != d || e >= f) {}
    "#, &lexer);

    run_test_case("Strings with escapes & unicode in strings", r#"
        string s = "line1\nline2\t\"quoted\" and emoji: 🌍";
    "#, &lexer);

    run_test_case("Numbers: ints, floats, scientific", r#"
        int a = 42;
        float f1 = 3.14;
        float f2 = 6.022e23;
        float f3 = 1.2E-3;
    "#, &lexer);

    run_test_case("Unicode identifiers", r#"
        fn int sum(int μ, int 树) {
            int 变量 = μ + 树;
            return 变量;
        }
    "#, &lexer);

    run_test_case("Invalid identifier starting with digit (should produce T_ERROR)", r#"
        int 123abc = 5;
    "#, &lexer);
}

