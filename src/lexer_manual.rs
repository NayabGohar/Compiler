

use std::fmt;

#[derive(Debug, Clone, PartialEq)]
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
    T_CHAR,
    T_DOUBLE,
    T_VOID,
    T_TRUE,
    T_FALSE,

    // Symbols / punctuation
    T_PARENL,   // (
    T_PARENR,   // )
    T_BRACEL,   // {
    T_BRACER,   // }
    T_BRACKETL, // [
    T_BRACKETR, // ]
    T_COMMA,    // ,
    T_SEMICOLON,// ;

    // Quotes marker around string literal
    T_QUOTES,   // "

    // Identifiers & literals
    T_IDENTIFIER(String),
    T_INTLIT(String),
    T_FLOATLIT(String),
    T_STRINGLIT(String),

    // Assignment and operators
    T_ASSIGNOP, // =
    T_EQUALSOP, // ==
    T_NOTEQUAL, // !=
    T_LEQ,      // <=
    T_GEQ,      // >=
    T_LT,       // <
    T_GT,       // >
    T_PLUS,     // +
    T_MINUS,    // -
    T_MUL,      // *
    T_DIV,      // /
    T_MOD,      // %
    T_AND,      // &&
    T_OR,       // ||
    T_NOT,      // !
    T_LSHIFT,   // <<
    T_RSHIFT,   // >>
    T_BITAND,   // &
    T_BITOR,    // |
    T_BITXOR,   // ^
    T_BITNOT,   // ~

    // Comments (kept as token optionally)
    T_COMMENT(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            T_IDENTIFIER(s) => write!(f, "T_IDENTIFIER(\"{}\")", s),
            T_INTLIT(s) => write!(f, "T_INTLIT({})", s),
            T_FLOATLIT(s) => write!(f, "T_FLOATLIT({})", s),
            T_STRINGLIT(s) => write!(f, "T_STRINGLIT(\"{}\")", s),
            T_COMMENT(s) => {
                let escaped = s.replace('\n', "\\n");
                write!(f, "T_COMMENT(\"{}\")", escaped)
            }
            other => write!(f, "{:?}", other),
        }
    }
}

/// Is character a valid identifier start: letter, underscore, or non-ascii (Unicode/emoji)
fn is_ident_start(ch: char) -> bool {
    ch == '_' || ch.is_alphabetic() || (ch as u32) >= 0x80
}

/// Is character valid to continue an identifier
fn is_ident_continue(ch: char) -> bool {
    ch == '_' || ch.is_alphanumeric() || (ch as u32) >= 0x80
}

/// Main lexer function (manual, no regex)
/// Returns Result<Vec<Token>, String> where Err contains an error message (with location info)
pub fn lexer_manual(code: &str) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = Vec::new();
    let chars: Vec<char> = code.chars().collect();
    let n = chars.len();
    let mut i: usize = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;

    macro_rules! advance {
        ($count:expr) => {{
            for _ in 0..$count {
                if i < n {
                    if chars[i] == '\n' { line += 1; col = 1; } else { col += 1; }
                    i += 1;
                }
            }
        }};
    }

    macro_rules! peek {
        ($off:expr) => {
            if i + $off < n { chars[i + $off] } else { '\0' }
        };
    }

    // helper to produce a position string
    let pos = |line: usize, col: usize| format!("line {}, col {}", line, col);

    while i < n {
        let ch = chars[i];

        // skip whitespace (space, tabs, newlines)
        if ch.is_whitespace() {
            advance!(1);
            continue;
        }

        // Comments: // single-line
        if ch == '/' && peek!(1) == '/' {
            let start_i = i;
            let start_line = line;
            let start_col = col;
            // collect until newline or EOF
            while i < n && chars[i] != '\n' {
                advance!(1);
            }
            let comment: String = chars[start_i..i].iter().collect();
            tokens.push(Token::T_COMMENT(comment));
            continue;
        }

        // Comments: /* ... */
        if ch == '/' && peek!(1) == '*' {
            let start_i = i;
            let start_line = line;
            let start_col = col;
            advance!(2); // consume /*
            let mut closed = false;
            while i + 1 < n {
                if chars[i] == '*' && chars[i+1] == '/' {
                    closed = true;
                    advance!(2);
                    break;
                } else {
                    advance!(1);
                }
            }
            if !closed {
                return Err(format!("Unterminated block comment at {}", pos(start_line, start_col)));
            }
            let comment: String = chars[start_i..i].iter().collect();
            tokens.push(Token::T_COMMENT(comment));
            continue;
        }

        // Strings: emits T_QUOTES, T_STRINGLIT(inner), T_QUOTES
        if ch == '"' {
            // opening quote
            tokens.push(Token::T_QUOTES);
            advance!(1); // skip opening "
            let mut buf = String::new();
            let start_line = line;
            let start_col = col;
            while i < n {
                let c = chars[i];
                if c == '"' {
                    break;
                }
                if c == '\\' {
                    // escape - ensure next char exists
                    if i + 1 >= n {
                        return Err(format!("Bad escape at EOF in string starting at {}", pos(start_line, start_col)));
                    }
                    // keep escapes verbatim (e.g. \n, \", \t) inside the literal
                    buf.push('\\');
                    buf.push(chars[i+1]);
                    advance!(2);
                    continue;
                } else {
                    buf.push(c);
                    advance!(1);
                }
            }
            if i >= n {
                return Err(format!("Unterminated string literal at {}", pos(start_line, start_col)));
            }
            // closing quote
            advance!(1);
            tokens.push(Token::T_STRINGLIT(buf));
            tokens.push(Token::T_QUOTES);
            continue;
        }

        // Numbers: integer, float, scientific
        if ch.is_ascii_digit() {
            let start_i = i;
            let start_line = line;
            let start_col = col;

            // integer part
            while i < n && chars[i].is_ascii_digit() {
                advance!(1);
            }

            let mut has_dot = false;
            let mut has_exp = false;

            // fractional part
            if i < n && chars[i] == '.' {
                if i + 1 < n && chars[i+1].is_ascii_digit() {
                    has_dot = true;
                    advance!(1); // consume '.'
                    while i < n && chars[i].is_ascii_digit() {
                        advance!(1);
                    }
                } else {
                    // dot not followed by digit -> treat previous as int and leave '.' to be tokenized later
                }
            }

            // exponent
            if i < n && (chars[i] == 'e' || chars[i] == 'E') {
                has_exp = true;
                advance!(1);
                if i < n && (chars[i] == '+' || chars[i] == '-') {
                    advance!(1);
                }
                // must have at least one digit in exponent
                if i >= n || !chars[i].is_ascii_digit() {
                    return Err(format!("Malformed exponent at {}", pos(start_line, start_col)));
                }
                while i < n && chars[i].is_ascii_digit() {
                    advance!(1);
                }
            }

            // If after numeric token there is a letter/underscore/Unicode start without separator => invalid identifier
            if i < n && is_ident_start(chars[i]) {
                // e.g. "123abc" is invalid per spec
                let mut j = i;
                while j < n && is_ident_continue(chars[j]) { j += 1; }
                let bad: String = chars[start_i..j].iter().collect();
                return Err(format!("Invalid identifier starting with number '{}' at {}", bad, pos(start_line, start_col)));
            }

            let lexeme: String = chars[start_i..i].iter().collect();
            if has_dot || has_exp {
                tokens.push(Token::T_FLOATLIT(lexeme));
            } else {
                tokens.push(Token::T_INTLIT(lexeme));
            }
            continue;
        }

        // Identifiers / Keywords (Unicode supported)
        if is_ident_start(ch) {
            let start_i = i;
            let start_line = line;
            let start_col = col;
            advance!(1);
            while i < n && is_ident_continue(chars[i]) {
                advance!(1);
            }
            let word: String = chars[start_i..i].iter().collect();

            match word.as_str() {
                "fn" => tokens.push(Token::T_FUNCTION),
                "int" => tokens.push(Token::T_INT),
                "float" => tokens.push(Token::T_FLOAT),
                "string" => tokens.push(Token::T_STRING),
                "bool" => tokens.push(Token::T_BOOL),
                "return" => tokens.push(Token::T_RETURN),
                "if" => tokens.push(Token::T_IF),
                "else" => tokens.push(Token::T_ELSE),
                "while" => tokens.push(Token::T_WHILE),
                "for" => tokens.push(Token::T_FOR),
                "char" => tokens.push(Token::T_CHAR),
                "double" => tokens.push(Token::T_DOUBLE),
                "void" => tokens.push(Token::T_VOID),
                "true" => tokens.push(Token::T_TRUE),
                "false" => tokens.push(Token::T_FALSE),
                _ => tokens.push(Token::T_IDENTIFIER(word)),
            }
            continue;
        }

        // Operators — try multi-character operators first
        // two-char ops
        if i + 1 < n {
            let two: String = vec![chars[i], chars[i+1]].iter().collect();
            match two.as_str() {
                "==" => { tokens.push(Token::T_EQUALSOP); advance!(2); continue; }
                "!=" => { tokens.push(Token::T_NOTEQUAL); advance!(2); continue; }
                "<=" => { tokens.push(Token::T_LEQ); advance!(2); continue; }
                ">=" => { tokens.push(Token::T_GEQ); advance!(2); continue; }
                "&&" => { tokens.push(Token::T_AND); advance!(2); continue; }
                "||" => { tokens.push(Token::T_OR); advance!(2); continue; }
                "<<" => { tokens.push(Token::T_LSHIFT); advance!(2); continue; }
                ">>" => { tokens.push(Token::T_RSHIFT); advance!(2); continue; }
                _ => {}
            }
        }

        // single-char operators and punctuation
        match ch {
            '=' => { tokens.push(Token::T_ASSIGNOP); advance!(1); continue; }
            '<' => { tokens.push(Token::T_LT); advance!(1); continue; }
            '>' => { tokens.push(Token::T_GT); advance!(1); continue; }
            '+' => { tokens.push(Token::T_PLUS); advance!(1); continue; }
            '-' => { tokens.push(Token::T_MINUS); advance!(1); continue; }
            '*' => { tokens.push(Token::T_MUL); advance!(1); continue; }
            '/' => { tokens.push(Token::T_DIV); advance!(1); continue; }
            '%' => { tokens.push(Token::T_MOD); advance!(1); continue; }
            '&' => { tokens.push(Token::T_BITAND); advance!(1); continue; }
            '|' => { tokens.push(Token::T_BITOR); advance!(1); continue; }
            '^' => { tokens.push(Token::T_BITXOR); advance!(1); continue; }
            '~' => { tokens.push(Token::T_BITNOT); advance!(1); continue; }
            '!' => { tokens.push(Token::T_NOT); advance!(1); continue; }

            '(' => { tokens.push(Token::T_PARENL); advance!(1); continue; }
            ')' => { tokens.push(Token::T_PARENR); advance!(1); continue; }
            '{' => { tokens.push(Token::T_BRACEL); advance!(1); continue; }
            '}' => { tokens.push(Token::T_BRACER); advance!(1); continue; }
            '[' => { tokens.push(Token::T_BRACKETL); advance!(1); continue; }
            ']' => { tokens.push(Token::T_BRACKETR); advance!(1); continue; }
            ',' => { tokens.push(Token::T_COMMA); advance!(1); continue; }
            ';' => { tokens.push(Token::T_SEMICOLON); advance!(1); continue; }

            _ => {
                return Err(format!("Unexpected character '{}' at {}", ch, pos(line, col)));
            }
        }
    }

    Ok(tokens)
}

fn main() {
    let tests: Vec<(&str, &str)> = vec![
        ("Sample program (from prompt)", r#"
fn int my_fn(int x, float y) {
    string my_str = "hmm";
    bool my_bool = x == 40;
    return x;
}
"#),
        ("Spaces & tabs & newlines", "   \n\tfn   int   a  ( ) ;\n"),
        ("Comments", r#"
// single line
fn main() {
    /* multiline
       comment */
    return 0;
}
"#),
        ("Operators & bitwise & shifts", r#"
a = b & c | d ^ ~e;
x = y << 2;
z = w >> 1;
if (a <= b && c != d || e >= f) {}
"#),
        ("Strings with escapes & unicode in strings", r#"
string s = "line1\nline2\t\"quoted\" and emoji: 😀";
"#),
        ("Numbers: ints, floats, scientific", r#"
int a = 42;
float f1 = 3.14;
float f2 = 6.022e23;
float f3 = 1.2E-3;
"#),
        ("Unicode identifiers", r#"
fn int 😀sum(int μ, int 树) {
  int 变量 = μ + 树;
  return 变量;
}
"#),
        ("Invalid identifier starting with digit (should error)", r#"
int 123abc = 5;
"#),
    ];

    for (name, code) in tests {
        println!("=== TEST: {} ===", name);
        match lexer_manual(code) {
            Ok(tokens) => {
                let out: Vec<String> = tokens.iter().map(|t| t.to_string()).collect();
                println!("[{}]\n", out.join(", "));
            }
            Err(e) => {
                println!("LEX ERROR: {}\n", e);
            }
        }
    }
}

