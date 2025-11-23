use std::fmt;
use std::collections::HashMap;

use crate::parser_regex::{Decl, FnDecl, Stmt, Expr};

#[derive(Debug, Clone)]
pub enum IRGenError {
    ReturnOutsideFunction,
    UnknownConstruct(String),
    Other(String),
}

#[derive(Debug, Clone)]
pub enum Operand {
    Temp(String),
    Local(String),
    IntLit(String),
    FloatLit(String),
    StrLit(String),
    BoolLit(bool),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Temp(s) | Operand::Local(s) => write!(f, "{}", s),
            Operand::IntLit(s) => write!(f, "{}", s),
            Operand::FloatLit(s) => write!(f, "{}", s),
            Operand::StrLit(s) => write!(f, "\"{}\"", s),
            Operand::BoolLit(b) => write!(f, "{}", if *b { "true" } else { "false" }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Label(String),
    Assign { dest: Operand, src: Operand },
    Binary { op: String, dest: Operand, lhs: Operand, rhs: Operand },
    Unary { op: String, dest: Operand, rhs: Operand },
    Param(Operand),
    Call { dest: Option<Operand>, name: String, nargs: usize },
    Return(Option<Operand>),
    Goto(String),
    IfGoto { cond: Operand, label: String },
    FuncBegin(String),
    FuncEnd(String),
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Instr::*;
        match self {
            Label(l) => writeln!(f, "{}:", l),
            Assign { dest, src } => writeln!(f, "{} = {}", dest, src),
            Binary { op, dest, lhs, rhs } => writeln!(f, "{} = {} {} {}", dest, lhs, op, rhs),
            Unary { op, dest, rhs } => writeln!(f, "{} = {} {}", dest, op, rhs),
            Param(o) => writeln!(f, "param {}", o),
            Call { dest, name, nargs } => {
                if let Some(d) = dest {
                    writeln!(f, "{} = call {} {}", d, name, nargs)
                } else {
                    writeln!(f, "call {} {}", name, nargs)
                }
            }
            Return(Some(o)) => writeln!(f, "return {}", o),
            Return(None) => writeln!(f, "return"),
            Goto(l) => writeln!(f, "goto {}", l),
            IfGoto { cond, label } => writeln!(f, "if {} goto {}", cond, label),
            FuncBegin(name) => writeln!(f, "func {} begin", name),
            FuncEnd(name) => writeln!(f, "func {} end", name),
        }
    }
}

pub struct FunctionIR {
    pub name: String,
    pub instrs: Vec<Instr>,
}

pub struct IRGenerator {
    temp_counter: usize,
    label_counter: usize,
    locals: HashMap<String, Operand>,
    pub errors: Vec<IRGenError>,
}

impl IRGenerator {
    pub fn new() -> Self {
        IRGenerator {
            temp_counter: 0,
            label_counter: 0,
            locals: HashMap::new(),
            errors: Vec::new(),
        }
    }

    fn new_temp(&mut self) -> Operand {
        let t = format!("t{}", self.temp_counter);
        self.temp_counter += 1;
        Operand::Temp(t)
    }

    fn local_name(ident: &str) -> String {
        format!("v_{}", ident)
    }

    fn new_label(&mut self, base: &str) -> String {
        let l = format!("{}_L{}", base, self.label_counter);
        self.label_counter += 1;
        l
    }

    pub fn generate(&mut self, prog: &Vec<Decl>) -> Vec<FunctionIR> {
        let mut out = Vec::new();

        for decl in prog.iter() {
            if let Decl::FnDecl(fd) = decl {
                out.push(self.generate_function(fd));
            }
        }

        out
    }

    fn generate_function(&mut self, f: &FnDecl) -> FunctionIR {
        self.locals.clear();
        self.temp_counter = 0; 
        let mut instrs = Vec::new();

        instrs.push(Instr::FuncBegin(f.ident.clone()));

        for p in &f.params {
            let lname = IRGenerator::local_name(&p.ident);
            let op = Operand::Local(lname.clone());
            self.locals.insert(p.ident.clone(), op.clone());
        }

        for s in &f.block {
            self.lower_stmt(s, &mut instrs);
        }

        instrs.push(Instr::FuncEnd(f.ident.clone()));

        FunctionIR {
            name: f.ident.clone(),
            instrs,
        }
    }

    fn lower_stmt(&mut self, s: &Stmt, out: &mut Vec<Instr>) {
        use Stmt::*;
        match s {
            Var(v) => {
                let lname = IRGenerator::local_name(&v.ident);
                let local_op = Operand::Local(lname.clone());
                self.locals.insert(v.ident.clone(), local_op.clone());

                if let Some(e) = &v.expr {
                    if let Some(val) = self.lower_expr(e, out) {
                        out.push(Instr::Assign { dest: local_op, src: val });
                    }
                }
            }
            ExprStmt(opt_e) => {
                if let Some(e) = opt_e {
                    let _ = self.lower_expr(e, out);
                }
            }
            Ret(opt_e) => {
                if let Some(e) = opt_e {
                    if let Some(val) = self.lower_expr(e, out) {
                        out.push(Instr::Return(Some(val)));
                    } else {
                        out.push(Instr::Return(None));
                    }
                } else {
                    out.push(Instr::Return(None));
                }
            }
            If { cond, then_block, else_block } => {
                let then_label = self.new_label("then");
                let else_label = self.new_label("else");
                let end_label = self.new_label("ifend");

                if let Some(cop) = self.lower_expr(cond, out) {
                    out.push(Instr::IfGoto { cond: cop, label: then_label.clone() });
                    out.push(Instr::Goto(else_label.clone()));

                    out.push(Instr::Label(then_label.clone()));
                    for st in then_block { self.lower_stmt(st, out); }
                    out.push(Instr::Goto(end_label.clone()));

                    out.push(Instr::Label(else_label.clone()));
                    if !else_block.is_empty() {
                        for st in else_block { self.lower_stmt(st, out); }
                    }
                    out.push(Instr::Label(end_label.clone()));
                } else {
                    self.errors.push(IRGenError::Other("if: couldn't lower condition".into()));
                }
            }
            While { cond, block } => {
                let start_label = self.new_label("while_start");
                let body_label = self.new_label("while_body");
                let end_label = self.new_label("while_end");

                out.push(Instr::Label(start_label.clone()));
                if let Some(cop) = self.lower_expr(cond, out) {
                    out.push(Instr::IfGoto { cond: cop.clone(), label: body_label.clone() });
                    out.push(Instr::Goto(end_label.clone()));

                    out.push(Instr::Label(body_label.clone()));
                    for st in block { self.lower_stmt(st, out); }
                    out.push(Instr::Goto(start_label.clone()));

                    out.push(Instr::Label(end_label.clone()));
                } else {
                    self.errors.push(IRGenError::Other("while: couldn't lower condition".into()));
                }
            }
            Block(inner) => {
                for st in inner { self.lower_stmt(st, out); }
            }
        }
    }

    fn lower_expr(&mut self, e: &Expr, out: &mut Vec<Instr>) -> Option<Operand> {
        use Expr::*;
        match e {
            Int(s) => Some(Operand::IntLit(s.clone())),
            Float(s) => Some(Operand::FloatLit(s.clone())),
            Str(s) => Some(Operand::StrLit(s.clone())),
            Bool(b) => Some(Operand::BoolLit(*b)),
            Ident(name) => {
                if let Some(op) = self.locals.get(name) {
                    Some(op.clone())
                } else {
                    let gl = Operand::Local(IRGenerator::local_name(name));
                    Some(gl)
                }
            }
            Unary { op, rhs } => {
                if let Some(r) = self.lower_expr(rhs, out) {
                    let dest = self.new_temp();
                    let opname = match op {
                        crate::lexer_regex::Token::T_MINUS => "-".to_string(),
                        crate::lexer_regex::Token::T_NOT => "!".to_string(),
                        _ => format!("{:?}", op),
                    };
                    out.push(Instr::Unary { op: opname, dest: dest.clone(), rhs: r });
                    Some(dest)
                } else { None }
            }
            Binary { op, lhs, rhs } => {
                let l = self.lower_expr(lhs, out)?;
                let r = self.lower_expr(rhs, out)?;
                let dest = self.new_temp();
                let opname = match op {
                    crate::lexer_regex::Token::T_PLUS => "+",
                    crate::lexer_regex::Token::T_MINUS => "-",
                    crate::lexer_regex::Token::T_MUL => "*",
                    crate::lexer_regex::Token::T_DIV => "/",
                    crate::lexer_regex::Token::T_EQUALSOP => "==",
                    crate::lexer_regex::Token::T_NOTEQUAL => "!=",
                    crate::lexer_regex::Token::T_LT => "<",
                    crate::lexer_regex::Token::T_GT => ">",
                    crate::lexer_regex::Token::T_LEQ => "<=",
                    crate::lexer_regex::Token::T_GEQ => ">=",
                    crate::lexer_regex::Token::T_AND => "&&",
                    crate::lexer_regex::Token::T_OR => "||",
                    crate::lexer_regex::Token::T_ASSIGNOP => "=",
                    _ => "op",
                }.to_string();

                if let crate::lexer_regex::Token::T_ASSIGNOP = op {
                    match lhs.as_ref() {
                        Expr::Ident(ident) => {
                            let lname = IRGenerator::local_name(ident.as_str());
                            let loperand = Operand::Local(lname.clone());
                            out.push(Instr::Assign { dest: loperand.clone(), src: r.clone() });
                            Some(loperand)
                        }
                        _ => {
                            self.errors.push(IRGenError::Other("assignment target not an identifier".into()));
                            out.push(Instr::Binary { op: opname, dest: dest.clone(), lhs: l, rhs: r });
                            Some(dest)
                        }
                    }
                } else {
                    out.push(Instr::Binary { op: opname, dest: dest.clone(), lhs: l, rhs: r });
                    Some(dest)
                }
            }
            Call { name, args } => {
                let mut arg_ops = Vec::new();
                for a in args {
                    if let Some(ao) = self.lower_expr(a, out) {
                        arg_ops.push(ao);
                    } else {
                        arg_ops.push(Operand::IntLit("0".into())); 
                    }
                }

                for ao in &arg_ops {
                    out.push(Instr::Param(ao.clone()));
                }

                let dest = self.new_temp();
                out.push(Instr::Call { dest: Some(dest.clone()), name: name.clone(), nargs: arg_ops.len() });
                Some(dest)
            }
        }
    }
}

