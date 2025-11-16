use crate::scope_analyzer::{ScopeAnalyzer, Symbol, ScopeError};
use crate::parser_regex::{Decl, Stmt, Expr, TypeTok, FnDecl};

#[derive(Debug, Clone)]
pub enum TypeChkError {
    TypeMismatch { expected: TypeTok, found: TypeTok },
    NonBooleanCond { context: String, found: TypeTok },
    UndefinedFunction { name: String },
    UndeclaredVariable { name: String },
}

pub struct TypeChecker<'a> {
    pub sa: &'a mut ScopeAnalyzer,
    pub errors: Vec<TypeChkError>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(sa: &'a mut ScopeAnalyzer) -> Self {
        Self { sa, errors: Vec::new() }
    }

    pub fn check_program(&mut self, prog: &Vec<Decl>) -> bool {
        for decl in prog.iter() {
            match decl {
                Decl::FnDecl(f) => self.check_function(f),
                Decl::VarDecl(v) => {
                    if let Some(expr) = &v.expr {
                        let t = self.infer_expr_type(expr);
                        if let Some(tt) = t {
                            if tt != v.type_tok {
                                self.errors.push(TypeChkError::TypeMismatch {
                                    expected: v.type_tok.clone(),
                                    found: tt,
                                });
                            }
                        }
                    }
                }
            }
        }
        !self.errors.is_empty()
    }

    fn check_function(&mut self, f: &FnDecl) {
        self.sa.push_scope();
        for param in &f.params {
            self.sa.insert_var(param.ident.clone(), Symbol::Var { type_tok: param.type_tok.clone() });
        }
        self.check_stmt_vec(&f.block);
        self.sa.pop_scope();
    }

    fn check_stmt_vec(&mut self, stmts: &Vec<Stmt>) {
        for s in stmts.iter() { self.check_stmt(s); }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        use crate::parser_regex::Stmt::*;
        match stmt {
            Var(v) => {
                if let Some(expr) = &v.expr {
                    let t = self.infer_expr_type(expr);
                    if let Some(tt) = t {
                        if tt != v.type_tok {
                            self.errors.push(TypeChkError::TypeMismatch {
                                expected: v.type_tok.clone(),
                                found: tt,
                            });
                        }
                    }
                }
                self.sa.insert_var(v.ident.clone(), Symbol::Var { type_tok: v.type_tok.clone() });
            }
            ExprStmt(e) | Ret(e) => {
                if let Some(ex) = e {
                    self.infer_expr_type(ex);
                }
            }
            If { cond, then_block, else_block } => {
                if let Some(tt) = self.infer_expr_type(cond) {
                    if tt != TypeTok::Bool {
                        self.errors.push(TypeChkError::NonBooleanCond { context: "if".to_string(), found: tt });
                    }
                }
                self.sa.push_scope();
                self.check_stmt_vec(then_block);
                self.sa.pop_scope();

                if !else_block.is_empty() {
                    self.sa.push_scope();
                    self.check_stmt_vec(else_block);
                    self.sa.pop_scope();
                }
            }
            While { cond, block } => {
                if let Some(tt) = self.infer_expr_type(cond) {
                    if tt != TypeTok::Bool {
                        self.errors.push(TypeChkError::NonBooleanCond { context: "while".to_string(), found: tt });
                    }
                }
                self.sa.push_scope();
                self.check_stmt_vec(block);
                self.sa.pop_scope();
            }
            Block(inner) => {
                self.sa.push_scope();
                self.check_stmt_vec(inner);
                self.sa.pop_scope();
            }
        }
    }

    fn infer_expr_type(&mut self, expr: &Expr) -> Option<TypeTok> {
        use crate::parser_regex::Expr::*;
        match expr {
            Int(_) => Some(TypeTok::Int),
            Float(_) => Some(TypeTok::Float),
            Bool(_) => Some(TypeTok::Bool),
            Str(_) => Some(TypeTok::String),
            Ident(name) => {
                match self.sa.get_var(name) {
                    Some(Symbol::Var { type_tok }) => Some(type_tok.clone()),
                    _ => { 
                        self.errors.push(TypeChkError::UndeclaredVariable { name: name.clone() });
                        None
                    }
                }
            }
            Unary { rhs, .. } => self.infer_expr_type(rhs),
            Binary { lhs, rhs, .. } => {
                let l = self.infer_expr_type(lhs);
                let r = self.infer_expr_type(rhs);
                if let (Some(lt), Some(rt)) = (l.clone(), r.clone()) {
                    if lt != rt {
                        self.errors.push(TypeChkError::TypeMismatch { expected: lt, found: rt });
                    }
                }
                l.or(r)
            }
            Call { name: _, args: _ } => {
                // Function call type inference skipped for brevity
                None
            }
        }
    }
}
