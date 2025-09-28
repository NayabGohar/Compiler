use lexer_project::lexer_regex::{Lexer, Token};
use std::fs::File;
use std::io::Write;

/// AST

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedEOF,
    FailedToFindToken(Token),
    ExpectedTypeToken,
    ExpectedIdentifier,
    UnexpectedToken(Token),
    ExpectedFloatLit,
    ExpectedIntLit,
    ExpectedStringLit,
    ExpectedBoolLit,
    ExpectedExpr,
}

#[derive(Debug, Clone)]
pub enum TypeTok {
    Int,
    Float,
    String,
    Bool,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub type_tok: TypeTok,
    pub ident: String,
}

#[derive(Debug, Clone)]
pub enum Decl {
    FnDecl(FnDecl),
    VarDecl(VarDecl),
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub type_tok: TypeTok,
    pub ident: String,
    pub params: Vec<Param>,
    pub block: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub type_tok: TypeTok,
    pub ident: String,
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Var(VarDecl),
    ExprStmt(Option<Expr>),
    Ret(Option<Expr>),
    If {
        cond: Expr,
        then_block: Vec<Stmt>,
        else_block: Vec<Stmt>,
    },
    While {
        cond: Expr,
        block: Vec<Stmt>,
    },
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(String),
    Float(String),
    Str(String),
    Bool(bool),
    Ident(String),
    Unary { op: Token, rhs: Box<Expr> },
    Binary { op: Token, lhs: Box<Expr>, rhs: Box<Expr> },
    Call { name: String, args: Vec<Expr> },
}

/// PARSER

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> Option<Token> {
        if self.pos < self.tokens.len() {
            let t = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(t)
        } else {
            None
        }
    }

    fn expect(&mut self, want: &Token) -> Result<(), ParseError> {
        match self.peek() {
            Some(t) if t == want => {
                self.next();
                Ok(())
            }
            Some(t) => Err(ParseError::UnexpectedToken(t.clone())),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn consume_identifier(&mut self) -> Result<String, ParseError> {
        match self.next() {
            Some(Token::T_IDENTIFIER(name)) => Ok(name),
            Some(t) => Err(ParseError::UnexpectedToken(t)),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    fn consume_type(&mut self) -> Result<TypeTok, ParseError> {
        match self.next() {
            Some(Token::T_INT) => Ok(TypeTok::Int),
            Some(Token::T_FLOAT) => Ok(TypeTok::Float),
            Some(Token::T_STRING) => Ok(TypeTok::String),
            Some(Token::T_BOOL) => Ok(TypeTok::Bool),
            Some(t) => Err(ParseError::UnexpectedToken(t)),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Decl>, ParseError> {
        let mut decls = Vec::new();
        while let Some(tok) = self.peek() {
            match tok {
                Token::T_FUNCTION => {
                    let f = self.parse_function_decl()?;
                    decls.push(Decl::FnDecl(f));
                }
                Token::T_INT | Token::T_FLOAT | Token::T_STRING | Token::T_BOOL => {
                    let v = self.parse_var_decl()?;
                    decls.push(Decl::VarDecl(v));
                }
                Token::T_COMMENT(_) => { self.next(); } 
                Token::T_ERROR(e) => return Err(ParseError::UnexpectedToken(Token::T_ERROR(e.clone()))),
                _ => {
                    let t = self.next().unwrap();
                    return Err(ParseError::FailedToFindToken(t));
                }
            }
        }
        Ok(decls)
    }

    fn parse_function_decl(&mut self) -> Result<FnDecl, ParseError> {
        self.expect(&Token::T_FUNCTION)?;
        let type_tok = self.consume_type()?;
        let ident = self.consume_identifier()?;
        self.expect(&Token::T_PARENL)?;
        let mut params = Vec::new();
        loop {
            match self.peek() {
                Some(Token::T_PARENR) => { self.next(); break; }
                Some(Token::T_INT) | Some(Token::T_FLOAT) | Some(Token::T_STRING) | Some(Token::T_BOOL) => {
                    let ptype = self.consume_type()?;
                    let pname = self.consume_identifier()?;
                    params.push(Param { type_tok: ptype, ident: pname });
                    match self.peek() {
                        Some(Token::T_COMMA) => { self.next(); continue; }
                        Some(Token::T_PARENR) => continue,
                        _ => continue,
                    }
                }
                Some(Token::T_COMMENT(_)) => { self.next(); continue; }
                Some(t) => return Err(ParseError::UnexpectedToken(t.clone())),
                None => return Err(ParseError::UnexpectedEOF),
            }
        }

        self.expect(&Token::T_BRACEL)?;
        let block = self.parse_block_stmts()?;
        Ok(FnDecl { type_tok, ident, params, block })
    }

    fn parse_var_decl(&mut self) -> Result<VarDecl, ParseError> {
        let type_tok = self.consume_type()?;
        let ident = self.consume_identifier()?;
        let mut expr = None;
        if let Some(Token::T_ASSIGNOP) = self.peek() {
            self.next();
            expr = Some(self.parse_expression_pratt(0)?);
        }
        match self.peek() {
            Some(Token::T_SEMICOLON) => { self.next(); }
            Some(t) => return Err(ParseError::UnexpectedToken(t.clone())),
            None => return Err(ParseError::UnexpectedEOF),
        }
        Ok(VarDecl { type_tok, ident, expr })
    }

    fn parse_block_stmts(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        loop {
            match self.peek() {
                Some(Token::T_BRACER) => { self.next(); break; }
                Some(Token::T_COMMENT(_)) => { self.next(); continue; }
                Some(Token::T_INT) | Some(Token::T_FLOAT) | Some(Token::T_STRING) | Some(Token::T_BOOL) => {
                    let v = self.parse_var_decl()?;
                    stmts.push(Stmt::Var(v));
                }
                Some(Token::T_RETURN) => {
                    self.next();
                    let ret_expr = if let Some(Token::T_SEMICOLON) = self.peek() {
                        None
                    } else {
                        Some(self.parse_expression_pratt(0)?)
                    };
                    self.expect(&Token::T_SEMICOLON)?;
                    stmts.push(Stmt::Ret(ret_expr));
                }
                Some(Token::T_IF) => {
                    self.next();
                    self.expect(&Token::T_PARENL)?;
                    let cond = self.parse_expression_pratt(0)?;
                    self.expect(&Token::T_PARENR)?;
                    self.expect(&Token::T_BRACEL)?;
                    let then_block = self.parse_block_stmts()?;
                    let else_block = if let Some(Token::T_ELSE) = self.peek() {
                        self.next();
                        self.expect(&Token::T_BRACEL)?;
                        self.parse_block_stmts()?
                    } else { vec![] };
                    stmts.push(Stmt::If { cond, then_block, else_block });
                }
                Some(Token::T_WHILE) => {
                    self.next();
                    self.expect(&Token::T_PARENL)?;
                    let cond = self.parse_expression_pratt(0)?;
                    self.expect(&Token::T_PARENR)?;
                    self.expect(&Token::T_BRACEL)?;
                    let block = self.parse_block_stmts()?;
                    stmts.push(Stmt::While { cond, block });
                }
                Some(Token::T_BRACEL) => {
                    self.next();
                    let inner = self.parse_block_stmts()?;
                    stmts.push(Stmt::Block(inner));
                }
                Some(Token::T_SEMICOLON) => {
                    self.next(); 
                }
                Some(_) => {
                    let e = self.parse_expression_pratt(0)?;
                    if let Some(Token::T_SEMICOLON) = self.peek() {
                        self.next();
                    }
                    stmts.push(Stmt::ExprStmt(Some(e)));
                }
                None => return Err(ParseError::UnexpectedEOF),
            }
        }
        Ok(stmts)
    }

    // Pratt parser 
    fn infix_binding_power(op: &Token) -> Option<(u8, bool)> {
        match op {
            Token::T_ASSIGNOP => Some((1, true)), // lowest, right-assoc
            Token::T_OR => Some((2, false)),
            Token::T_AND => Some((3, false)),
            Token::T_EQUALSOP | Token::T_NOTEQUAL => Some((4, false)),
            Token::T_LT | Token::T_LEQ | Token::T_GT | Token::T_GEQ => Some((5, false)),
            Token::T_PLUS | Token::T_MINUS => Some((6, false)),
            Token::T_MUL | Token::T_DIV => Some((7, false)),
            _ => None,
        }
    }

    fn parse_expression_pratt(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        //  prefix
        let mut lhs = match self.next() {
            Some(Token::T_INTLIT(s)) => Expr::Int(s),
            Some(Token::T_FLOATLIT(s)) => Expr::Float(s),
            Some(Token::T_STRINGLIT(s)) => Expr::Str(s),
            Some(Token::T_IDENTIFIER(name)) => {
                if let Some(Token::T_PARENL) = self.peek() {
                    self.next(); 
                    let mut args = Vec::new();
                    loop {
                        match self.peek() {
                            Some(Token::T_PARENR) => { self.next(); break; }
                            Some(_) => {
                                let arg = self.parse_expression_pratt(0)?;
                                args.push(arg);
                                match self.peek() {
                                    Some(Token::T_COMMA) => { self.next(); continue; }
                                    Some(Token::T_PARENR) => continue,
                                    _ => continue,
                                }
                            }
                            None => return Err(ParseError::UnexpectedEOF),
                        }
                    }
                    Expr::Call { name, args }
                } else {
                    Expr::Ident(name)
                }
            }
            Some(Token::T_PARENL) => {
                let e = self.parse_expression_pratt(0)?;
                self.expect(&Token::T_PARENR)?;
                e
            }
            Some(Token::T_MINUS) => {
                let rhs = self.parse_expression_pratt(8)?; 
                Expr::Unary { op: Token::T_MINUS, rhs: Box::new(rhs) }
            }
            Some(Token::T_NOT) => {
                let rhs = self.parse_expression_pratt(8)?;
                Expr::Unary { op: Token::T_NOT, rhs: Box::new(rhs) }
            }
            Some(t) => return Err(ParseError::UnexpectedToken(t)),
            None => return Err(ParseError::UnexpectedEOF),
        };

        loop {
            let op = match self.peek() {
                Some(t) => t.clone(),
                None => break,
            };

            if let Some((bp, right_assoc)) = Parser::infix_binding_power(&op) {
                let (left_bp, right_bp) = if right_assoc { (bp, bp-1) } else { (bp, bp) };
                if left_bp < min_bp { break; }
                let op_tok = self.next().unwrap();
                let next_min = right_bp + 1;
                let rhs = self.parse_expression_pratt(next_min)?;
                lhs = Expr::Binary { op: op_tok, lhs: Box::new(lhs), rhs: Box::new(rhs) };
                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

/// BNF 
fn write_bnf_file() {
    let bnf = r#"
# Language BNF (simple, partial)

<program> ::= { <declaration> }

<declaration> ::= <function-decl> | <var-decl>

<function-decl> ::= "fn" <type> <identifier> "(" [ <param-list> ] ")" "{" { <statement> } "}"

<param-list> ::= <type> <identifier> { "," <type> <identifier> }

<var-decl> ::= <type> <identifier> [ "=" <expression> ] ";"

<statement> ::= <var-decl>
              | "return" [ <expression> ] ";"
              | "if" "(" <expression> ")" "{" { <statement> } "}" [ "else" "{" { <statement> } "}" ]
              | "while" "(" <expression> ")" "{" { <statement> } "}"
              | <expression> ";"
              | "{" { <statement> } "}"

<expression> ::= <assignment>
<assignment> ::= <logical-or> [ "=" <assignment> ]
<logical-or> ::= <logical-and> { "||" <logical-and> }
<logical-and> ::= <equality> { "&&" <equality> }
<equality> ::= <comparison> { ("==" | "!=") <comparison> }
<comparison> ::= <term> { ("<" | "<=" | ">" | ">=") <term> }
<term> ::= <factor> { ("+" | "-") <factor> }
<factor> ::= <unary> { ("*" | "/") <unary> }
<unary> ::= ("!" | "-") <unary> | <call>
<call> ::= <primary> { "(" [ <arg-list> ] ")" }
<primary> ::= <integer> | <float> | <string> | <identifier> | "(" <expression> ")"

<arg-list> ::= <expression> { "," <expression> }
<type> ::= "int" | "float" | "string" | "bool"
"#;

    if let Ok(mut f) = File::create("language_bnf.md") {
        let _ = f.write_all(bnf.as_bytes());
    }
}

///MAIN

fn run_and_print(input: &str) {
    let lexer = Lexer::new();
    let tokens = lexer.tokenize(input);

    println!("Tokens: {:?}", tokens);

    let mut parser = Parser::new(tokens.clone());
    match parser.parse_program() {
        Ok(ast) => {
            println!("AST: {:#?}", ast);
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}

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
            int x = 1 + 2 * 3;
            int y = (1 + 2) * 3;
            int z = 1 + 2 + 3;
            if (x == y) { return x; } else { return y; }
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