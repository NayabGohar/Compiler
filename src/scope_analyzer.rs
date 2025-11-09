use std::collections::HashMap;
use crate::parser_regex::{FnDecl, VarDecl, Param, Stmt, Expr, TypeTok, Decl};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopeError {
    UndeclaredVariableAccessed { name: String },
    UndefinedFunctionCalled { name: String },
    VariableRedefinition { name: String },
    FunctionPrototypeRedefinition { name: String },
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Var { type_tok: crate::TypeTok },
    Fn {
        return_type: crate::TypeTok,
        params: Vec<crate::Param>,
    },
}

pub struct ScopeAnalyzer {
    scopes: Vec<HashMap<String, Symbol>>,
    functions: HashMap<String, Symbol>,
    pub errors: Vec<ScopeError>,
}

impl ScopeAnalyzer {
    pub fn new() -> Self {
        ScopeAnalyzer {
            scopes: Vec::new(),
            functions: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn analyze(&mut self, prog: &Vec<crate::Decl>) -> bool {
        println!("Starting scope analysis");
        self.enter_scope(); // global

        println!(" Registering top-level declarations");
        for decl in prog.iter() {
            match decl {
                crate::Decl::FnDecl(f) => {
                    let name = f.ident.clone();
                    if self.functions.contains_key(&name) {
                        println!(" Function redefinition detected: {}", name);
                        self.errors.push(ScopeError::FunctionPrototypeRedefinition { name });
                    } else {
                        println!("Declared function: {}", name);
                        let sym = Symbol::Fn {
                            return_type: f.type_tok.clone(),
                            params: f.params.clone(),
                        };
                        self.functions.insert(name, sym);
                    }
                }
                crate::Decl::VarDecl(v) => {
                    let name = v.ident.clone();
                    if self.current_scope_contains(&name) {
                        println!(" Global variable redefinition: {}", name);
                        self.errors.push(ScopeError::VariableRedefinition { name });
                    } else {
                        println!(" Declared global variable: {}", name);
                        self.insert_in_current_scope(name, Symbol::Var { type_tok: v.type_tok.clone() });
                    }
                }
            }
        }

        println!("\nAnalyzing function bodies");
        for decl in prog.iter() {
            if let crate::Decl::FnDecl(f) = decl {
                self.analyze_function(f);
            }
        }

        self.exit_scope(); // global
        println!("\n Scope analysis complete!");
        !self.errors.is_empty()
    }

    fn analyze_function(&mut self, f: &crate::FnDecl) {
        println!("\n Entering function: {}", f.ident);
        self.enter_scope();

        for p in &f.params {
            if self.current_scope_contains(&p.ident) {
                println!(" Parameter redefinition: {}", p.ident);
                self.errors.push(ScopeError::VariableRedefinition { name: p.ident.clone() });
            } else {
                println!(" Param: {} ({:?})", p.ident, p.type_tok);
                self.insert_in_current_scope(p.ident.clone(), Symbol::Var { type_tok: p.type_tok.clone() });
            }
        }

        self.analyze_stmt_vec(&f.block);

        self.exit_scope();
        println!(" Exiting function: {}", f.ident);
    }

    fn analyze_stmt_vec(&mut self, stmts: &Vec<crate::Stmt>) {
        for s in stmts.iter() {
            self.analyze_stmt(s);
        }
    }

    fn analyze_stmt(&mut self, stmt: &crate::Stmt) {
        use crate::Stmt::*;
        match stmt {
            Stmt::Var(v) => {
                if self.current_scope_contains(&v.ident) {
                    println!(" Variable redefinition in same scope: {}", v.ident);
                    self.errors.push(ScopeError::VariableRedefinition { name: v.ident.clone() });
                } else {
                    if let Some(expr) = &v.expr {
                        self.analyze_expr(expr);
                    }
                    println!(" Declared local variable: {}", v.ident);
                    self.insert_in_current_scope(v.ident.clone(), Symbol::Var { type_tok: v.type_tok.clone() });
                }
            }
            Stmt::ExprStmt(eopt) => {
                if let Some(e) = eopt {
                    self.analyze_expr(e);
                }
            }
            Stmt::Ret(eopt) => {
                if let Some(e) = eopt {
                    self.analyze_expr(e);
                }
            }
            Stmt::If { cond, then_block, else_block } => {
                println!(" If-statement: analyzing condition");
                self.analyze_expr(cond);
                println!(" Entering then-block");
                self.enter_scope();
                self.analyze_stmt_vec(then_block);
                self.exit_scope();
                println!("Exiting then-block");

                if !else_block.is_empty() {
                    println!(" Entering else-block");
                    self.enter_scope();
                    self.analyze_stmt_vec(else_block);
                    self.exit_scope();
                    println!(" Exiting else-block");
                }
            }
            Stmt::While { cond, block } => {
                println!(" While-loop: condition check");
                self.analyze_expr(cond);
                self.enter_scope();
                self.analyze_stmt_vec(block);
                self.exit_scope();
            }
            Stmt::Block(inner) => {
                println!(" New block entered");
                self.enter_scope();
                self.analyze_stmt_vec(inner);
                self.exit_scope();
                println!("Block exited");
            }
        }
    }

    fn analyze_expr(&mut self, expr: &crate::Expr) {
        use crate::Expr::*;
        match expr {
            Int(_) | Float(_) | Str(_) | Bool(_) => {}
            Ident(name) => {
                if self.find_var(name).is_none() {
                    println!(" Undeclared variable used: {}", name);
                    self.errors.push(ScopeError::UndeclaredVariableAccessed { name: name.clone() });
                } else {
                    println!(" Identifier resolved: {}", name);
                }
            }
            Unary { rhs, .. } => {
                self.analyze_expr(rhs);
            }
            Binary { lhs, rhs, .. } => {
                self.analyze_expr(lhs);
                self.analyze_expr(rhs);
            }
            Call { name, args } => {
                if !self.functions.contains_key(name) {
                    println!(" Undefined function called: {}", name);
                    self.errors.push(ScopeError::UndefinedFunctionCalled { name: name.clone() });
                } else {
                    println!(" Function call resolved: {}", name);
                }
                for arg in args {
                    self.analyze_expr(arg);
                }
            }
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
        println!(" Entered new scope (depth = {})", self.scopes.len());
        self.print_scope_stack();
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
        println!(" Exited scope (remaining depth = {})", self.scopes.len());
        self.print_scope_stack();
    }

    fn insert_in_current_scope(&mut self, name: String, sym: Symbol) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, sym);
        }
    }

    fn current_scope_contains(&self, name: &str) -> bool {
        self.scopes.last().map_or(false, |s| s.contains_key(name))
    }

    fn find_var(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get(name) {
                if matches!(sym, Symbol::Var { .. }) {
                    return Some(sym);
                }
            }
        }
        None
    }

    /// 🪡 Print the spaghetti stack for visualization
    fn print_scope_stack(&self) {
        println!(" Spaghetti stack (innermost last):");
        for (i, scope) in self.scopes.iter().enumerate() {
            println!("  Scope[{}]: {:?}", i, scope.keys().collect::<Vec<_>>());
        }
        println!();
    }
}
