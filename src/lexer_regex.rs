use regex::Regex;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // Keywords
    T_FUNCTION,
    T_INT,
    T_FLOAT,
    T_STRING,
    T_BOOL,
    T_RETURN,
    T_IF,
    T_ELSE,
    T_WHILE,
    T_FOR,

    // Identifiers
    T_IDENTIFIER(String),

    // Literals
    T_INTLIT(String),
    T_FLOATLIT(String),
    T_STRINGLIT(String),

    // Punctuation
    T_PARENL,  // (
    T_PARENR,  // )
    T_BRACEL,  // {
    T_BRACER,  // }
    T_BRACKETL, // [
    T_BRACKETR, // ]
    T_COMMA,
    T_SEMICOLON,
    T_QUOTES,

    // Operators
    T_ASSIGNOP,
    T_EQUALSOP,
    T_NOTEQUAL,
    T_LEQ,
    T_GEQ,
    T_AND,
    T_OR,
    T_PLUS,
    T_MINUS,
    T_MUL,
    T_DIV,

    // Comments
    T_COMMENT(String),

    // Error
    T_ERROR(String),
}

pub struct Lexer {
    patterns: Vec<(Regex, fn(&str) -> Token)>,
    keywords: HashMap<&'static str, Token>,
}

impl Lexer {
    pub fn new() -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("fn", Token::T_FUNCTION);
        keywords.insert("int", Token::T_INT);
        keywords.insert("float", Token::T_FLOAT);
        keywords.insert("string", Token::T_STRING);
        keywords.insert("bool", Token::T_BOOL);
        keywords.insert("return", Token::T_RETURN);
        keywords.insert("if", Token::T_IF);
        keywords.insert("else", Token::T_ELSE);
        keywords.insert("while", Token::T_WHILE);
        keywords.insert("for", Token::T_FOR);

        let patterns: Vec<(Regex, fn(&str) -> Token)> = vec![
            // Comments
            (Regex::new(r"^//[^\n]*").unwrap(), |s| Token::T_COMMENT(s.to_string())),
            (Regex::new(r"^/\*[\s\S]*?\*/").unwrap(), |s| Token::T_COMMENT(s.to_string())),

            // Strings with escapes + unicode
            (Regex::new(r#"^"((?:\\.|[^"\\])*)""#).unwrap(), |s| {
                let inner = &s[1..s.len()-1];
                Token::T_STRINGLIT(inner.to_string())
            }),

            // Floats and scientific notation
            (Regex::new(r"^\d+\.\d+(?:[eE][+-]?\d+)?").unwrap(), |s| Token::T_FLOATLIT(s.to_string())),
            (Regex::new(r"^\d+[eE][+-]?\d+").unwrap(), |s| Token::T_FLOATLIT(s.to_string())),

            // Integers
            (Regex::new(r"^\d+").unwrap(), |s| Token::T_INTLIT(s.to_string())),

            // Identifiers (unicode allowed)
            (Regex::new(r"^[\p{L}_][\p{L}\p{N}_]*").unwrap(), |s| Token::T_IDENTIFIER(s.to_string())),

            // Operators
            (Regex::new(r"^==").unwrap(), |_| Token::T_EQUALSOP),
            (Regex::new(r"^!=").unwrap(), |_| Token::T_NOTEQUAL),
            (Regex::new(r"^<=").unwrap(), |_| Token::T_LEQ),
            (Regex::new(r"^>=").unwrap(), |_| Token::T_GEQ),
            (Regex::new(r"^&&").unwrap(), |_| Token::T_AND),
            (Regex::new(r"^\|\|").unwrap(), |_| Token::T_OR),
            (Regex::new(r"^=").unwrap(), |_| Token::T_ASSIGNOP),
            (Regex::new(r"^\+").unwrap(), |_| Token::T_PLUS),
            (Regex::new(r"^-").unwrap(), |_| Token::T_MINUS),
            (Regex::new(r"^\*").unwrap(), |_| Token::T_MUL),
            (Regex::new(r"^/").unwrap(), |_| Token::T_DIV),

            // Punctuation
            (Regex::new(r"^\(").unwrap(), |_| Token::T_PARENL),
            (Regex::new(r"^\)").unwrap(), |_| Token::T_PARENR),
            (Regex::new(r"^\{").unwrap(), |_| Token::T_BRACEL),
            (Regex::new(r"^\}").unwrap(), |_| Token::T_BRACER),
            (Regex::new(r"^\[").unwrap(), |_| Token::T_BRACKETL),
            (Regex::new(r"^\]").unwrap(), |_| Token::T_BRACKETR),
            (Regex::new(r"^,").unwrap(), |_| Token::T_COMMA),
            (Regex::new(r"^;").unwrap(), |_| Token::T_SEMICOLON),
            (Regex::new(r#"^""#).unwrap(), |_| Token::T_QUOTES),
        ];

        Lexer { patterns, keywords }
    }

    pub fn tokenize(&self, input: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut s = input.trim_start().to_string();

        while !s.is_empty() {
            let mut matched = false;

            for (re, action) in &self.patterns {
                if let Some(m) = re.find(&s) {
                    let lexeme = m.as_str();

                    // Skip whitespace
                    if lexeme.trim().is_empty() {
                        s = s[m.end()..].trim_start().to_string();
                        matched = true;
                        break;
                    }

                    // Handle keywords
                    if let Token::T_IDENTIFIER(ref name) = action(lexeme) {
                        if let Some(keyword) = self.keywords.get(name.as_str()) {
                            tokens.push(keyword.clone());
                        } else {
                            // Invalid identifiers starting with digit
                            if name.chars().next().unwrap().is_ascii_digit() {
                                tokens.push(Token::T_ERROR(format!("Invalid identifier starting with number '{}'", name)));
                            } else {
                                tokens.push(Token::T_IDENTIFIER(name.clone()));
                            }
                        }
                    } else {
                        tokens.push(action(lexeme));
                    }

                    s = s[m.end()..].trim_start().to_string();
                    matched = true;
                    break;
                }
            }

            if !matched {
                let ch = s.chars().next().unwrap();
                tokens.push(Token::T_ERROR(format!("Unexpected character '{}'", ch)));
                s = s[ch.len_utf8()..].trim_start().to_string();
            }
        }

        tokens
    }
}

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
