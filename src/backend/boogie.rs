mod ast;
mod emit;
use std::fmt::Display;

use crate::frontend::type_checker::*;

fn bail() -> ast::Ident {
    ast::Ident("bail".to_string())
}
fn ret() -> ast::Ident {
    ast::Ident("ret".to_string())
}
fn true_ident() -> ast::Ident {
    ast::Ident("true".to_string())
}
fn false_ident() -> ast::Ident {
    ast::Ident("false".to_string())
}
fn unit() -> ast::Ident {
    ast::Ident("unit".to_string())
}

fn struct_ident(env: &Env, idx: StructIndex) -> ast::Ident {
    user_ident(&env.struct_defs[idx].ident)
}

fn user_ident(name: &impl Display) -> ast::Ident {
    ast::Ident(format!("${}", name))
}

fn const_ident(env: &Env, idx: ConstIndex) -> ast::Ident {
    let c = &env.const_defs[idx];
    match c.parent_type {
        Some(t) => ast::Ident(format!("{}~{}", type_name(env, t), c.ident.to_string())),
        None => user_ident(&c.ident),
    }
}

fn fn_ident(env: &Env, idx: FnIndex) -> ast::Ident {
    let f = &env.fn_defs[idx];
    match f.parent_type {
        Some(t) => ast::Ident(format!("{}~{}", type_name(env, t), f.sig.ident.to_string())),
        None => user_ident(&f.sig.ident),
    }
}

fn type_name(env: &Env, idx: TypeIndex) -> ast::Ident {
    match idx {
        TypeIndex::BuiltIn(t) => match t {
            BuiltInType::Unit => ast::Ident("Unit".to_string()),
            BuiltInType::Bool => ast::Ident("Bool".to_string()),
            BuiltInType::Int32 => ast::Ident("Int32".to_string()),
            BuiltInType::Double => ast::Ident("Double".to_string()),
        },
        TypeIndex::Enum(idx) => enum_ident(env, idx),
        TypeIndex::Struct(idx) => struct_ident(env, idx),
    }
}

fn type_ident(env: &Env, idx: TypeIndex) -> ast::Ident {
    match idx {
        TypeIndex::Struct(_) => ast::Ident("Ref".to_string()),
        _ => type_name(env, idx),
    }
}

fn enum_ident(env: &Env, idx: EnumIndex) -> ast::Ident {
    user_ident(&env.enum_defs[idx].ident)
}

fn variant_ident(env: &Env, idx: EnumIndex, variant: VariantIndex) -> ast::Ident {
    ast::Ident(format!(
        "{}~{}",
        enum_ident(env, idx),
        env.enum_defs[idx].variants[variant]
    ))
}

pub fn lower_env<'a>(env: &'a Env) -> ast::Program {
    let preamble = ast::Decl::Hack(r#"
type Heap;
type Ref;
var heap: Heap;
procedure ObserveHeapWrite();
    modifies heap;

var bail: bool;
type {:datatype} Unit;
function {:constructor} unit(): Unit;
type Bool = bool;
type Int32 = bv32;
type Double = float53e11; // 64-bit; see https://github.com/boogie-org/boogie/issues/29#issuecomment-231239065

function {:bvbuiltin "bvadd"} Int32^Add(x: Int32, y: Int32): Int32;
"#.to_string());
    let type_decls = env
        .type_def_order
        .iter()
        .map(|td| -> Box<dyn Iterator<Item = ast::Decl>> {
            match td {
                TypeIndex::BuiltIn(_) => Box::new(std::iter::empty()),
                TypeIndex::Enum(idx) => Box::new(lower_enum(env, *idx)),
                TypeIndex::Struct(idx) => Box::new(lower_struct(env, *idx)),
            }
        })
        .flatten();

    let const_decls = env
        .const_defs
        .iter()
        .enumerate()
        .map(|(idx, _)| lower_const(env, idx))
        .flatten();

    let fn_decls = env.fn_def_order.iter().flat_map(|idx| lower_fn(env, *idx));

    let op_decls = env.op_defs.iter().flat_map(|op| lower_op(env, op));

    ast::Program(
        std::iter::once(preamble)
            .chain(type_decls)
            .chain(fn_decls)
            .chain(const_decls)
            .chain(op_decls)
            .collect(),
    )
}

fn lower_enum<'a>(env: &'a Env, idx: EnumIndex) -> impl Iterator<Item = ast::Decl> + 'a {
    let type_decl = ast::Decl::Type {
        name: enum_ident(env, idx),
        keyword: Some("datatype".into()),
    };

    let variants = env.enum_defs[idx]
        .variants
        .iter()
        .enumerate()
        .map(move |(v_i, _)| ast::Decl::Func {
            name: variant_ident(env, idx, v_i),
            params: vec![],
            return_type: enum_ident(env, idx),
            keyword: Some("constructor".into()),
        });

    std::iter::once(type_decl).chain(variants)
}

fn lower_struct<'a>(env: &'a Env, idx: StructIndex) -> impl Iterator<Item = ast::Decl> {
    let type_decl = ast::Decl::Type {
        name: struct_ident(env, idx),
        keyword: None,
    };

    std::iter::once(type_decl)
}

fn lower_const(env: &Env, idx: ConstIndex) -> impl Iterator<Item = ast::Decl> {
    let const_decl = ast::Decl::Const(ast::TypedVar {
        var: const_ident(env, idx),
        typ: type_ident(env, env.const_defs[idx].type_),
    });

    std::iter::once(const_decl)
}

fn lower_op(env: &Env, op: &OpDef) -> Vec<ast::Decl> {
    lower_fn_or_op(env, user_ident(&op.sig.ident), op, &op.sig, Some(&op.body))
}

fn lower_fn_or_op(
    env: &Env,
    name: ast::Ident,
    scope: &dyn Scope,
    sig: &Sig,
    body: Option<&Body>,
) -> Vec<ast::Decl> {
    let mut decls = vec![];
    let mut params = vec![];
    let mut returns = vec![];
    for p in &sig.param_vars {
        if p.is_out {
            returns.push(lower_param_var(env, p))
        } else {
            params.push(lower_param_var(env, p))
        }
    }
    returns.push(ast::TypedVar {
        var: ret(),
        typ: type_ident(env, sig.ret),
    });
    if body.is_none() {
        let mut body = vec![];
        for out_var in returns.iter() {
            let pure_ident = fresh_ident("pure");
            let pure_fn = ast::Decl::Func {
                name: pure_ident.clone(),
                params: params.clone(),
                return_type: out_var.typ.clone(),
                keyword: None,
            };

            decls.push(pure_fn);
            body.push(ast::Statement::Assign {
                var: out_var.var.clone(),
                val: ast::Expr::Call {
                    func: pure_ident,
                    args: params
                        .iter()
                        .map(|pv| ast::Expr::Var(pv.var.clone()))
                        .collect(),
                },
            })
        }
        let proc = ast::Decl::Proc {
            name,
            params,
            returns,
            modifies: vec![],
            ensures: vec![],
            body: Some(body),
        };
        decls.push(proc);
    } else {
        decls.push(ast::Decl::Proc {
            name,
            params,
            returns,
            modifies: body.is_some().then(|| bail()).into_iter().collect(),
            ensures: if !sig.is_fallible && body.is_some() {
                vec![ast::Ensure {
                    is_free: true,
                    expr: ast::Expr::Neg(Box::new(ast::Expr::Var(bail()))),
                }]
            } else {
                vec![]
            },
            body: body.as_ref().map(|b| lower_body(env, scope, b)),
        })
    }
    decls
}

fn lower_fn(env: &Env, idx: FnIndex) -> Vec<ast::Decl> {
    let f = &env.fn_defs[idx];
    lower_fn_or_op(env, fn_ident(env, idx), f, &f.sig, f.body.as_ref())
}

fn lower_body(env: &Env, scope: &dyn Scope, body: &Body) -> Vec<ast::Statement> {
    let ret = ast::Ident("ret".to_string());

    let vars = body.local_vars.iter().map(|lv| {
        ast::Statement::Var(ast::TypedVar {
            var: user_ident(&lv.ident),
            typ: type_ident(env, lv.type_),
        })
    });
    let mut bl = BodyLowerer {
        env,
        scope,
        stmts: vec![],
    };
    bl.lower_block(Fallible, &body.block, ret);
    vars.chain(bl.stmts.into_iter()).collect()
}

#[derive(Clone, Copy, Debug)]
enum AssertMode {
    Fallible,
    Infallible,
}
use AssertMode::*;

struct BodyLowerer<'a> {
    env: &'a Env,
    scope: &'a dyn Scope,
    stmts: Vec<ast::Statement>,
}

impl BodyLowerer<'_> {
    fn new_var(&mut self, hint: &str, ty: TypeIndex) -> ast::Ident {
        let ident = {
            let hint = hint;
            static mut IDENT_CT: u32 = 0;
            let index = unsafe {
                IDENT_CT += 1;
                IDENT_CT
            };

            ast::Ident(format!("gen_{}_{}", index, hint))
        };
        self.stmts.insert(
            0,
            ast::Statement::Var(ast::TypedVar {
                var: ident.clone(),
                typ: type_ident(self.env, ty),
            }),
        );
        ident
    }

    fn lower_block<'a>(&mut self, mode: AssertMode, block: &'a Block, ret_name: ast::Ident) {
        for stmt in &block.stmts {
            self.lower_stmt(mode, stmt);
        }

        let expr = self.lower_expr(mode, &block.value);

        let assign = ast::Statement::Assign {
            var: ret_name,
            val: expr,
        };

        self.stmts.push(assign);
    }

    fn lower_stmt(&mut self, mode: AssertMode, s: &Stmt) {
        match s {
            Stmt::Let(let_stmt) => {
                let expr = self.lower_expr(mode, &let_stmt.rhs);
                self.stmts.push(ast::Statement::Assign {
                    var: user_ident(&self.scope.lookup(ScopedVarIndex::LocalVar(let_stmt.lhs))),
                    val: expr,
                });
            }
            Stmt::Expr(e) => {
                self.lower_expr(mode, e);
            }
            Stmt::Check(check_stmt) => match check_stmt.kind {
                CheckStmtKind::Assert => todo!(),
                CheckStmtKind::Guard => {
                    let cond = self.lower_expr(mode, &check_stmt.cond);
                    self.stmts.push(ast::Statement::If {
                        cond,
                        body: vec![
                            ast::Statement::Assign {
                                var: bail(),
                                val: ast::Expr::Var(true_ident()),
                            },
                            ast::Statement::Return,
                        ],
                        els: vec![],
                    });
                }
            },
        }
    }

    fn lower_expr(&mut self, mode: AssertMode, expr: &Expr) -> ast::Expr {
        match expr {
            Expr::Block(block) => {
                let block_var = self.new_var("block_var", block.type_());
                let mode = if block.kind == Some(BlockExprKind::Fallible) {
                    AssertMode::Infallible
                } else {
                    mode
                };

                self.lower_block(mode, &block.block, block_var.clone());
                ast::Expr::Var(block_var)
            }
            Expr::Var(var_expr) => match var_expr.index {
                VarIndex::BuiltIn(b) => match b {
                    BuiltInVar::Unit => ast::Expr::Call {
                        func: unit(),
                        args: vec![],
                    },
                    BuiltInVar::True => ast::Expr::Var(true_ident()),
                    BuiltInVar::False => ast::Expr::Var(false_ident()),
                },
                VarIndex::EnumVariant(idx) => ast::Expr::Call {
                    func: variant_ident(self.env, idx.enum_index, idx.variant_index),
                    args: vec![],
                },
                VarIndex::Const(idx) => ast::Expr::Var(const_ident(self.env, idx)),
                VarIndex::ScopedVar(sv) => ast::Expr::Var(user_ident(&self.scope.lookup(sv))),
            },

            Expr::Call(call_expr) => {
                let result = self.new_var("call_result", call_expr.type_());
                let mut arg_exprs = vec![];
                let mut returns = vec![];
                for arg in call_expr.args.iter() {
                    match arg {
                        CallExprArg::Expr(expr) => {
                            let arg_expr = self.lower_expr(mode, expr);
                            arg_exprs.push(arg_expr);
                        }
                        CallExprArg::OutRef(arg) => {
                            returns.push(user_ident(&self.scope.lookup(arg.index)));
                        }
                    }
                }

                returns.push(result.clone());

                let call = ast::Statement::ProcCall {
                    returns,
                    proc: fn_ident(self.env, call_expr.target),
                    args: arg_exprs,
                };

                self.stmts.push(call);

                if call_expr.is_fallible {
                    match mode {
                        Fallible => self.stmts.push(ast::Statement::If {
                            cond: ast::Expr::BinOp {
                                op: ast::Op::EQ,
                                lhs: Box::new(ast::Expr::Var(bail())),
                                rhs: Box::new(ast::Expr::Var(true_ident())),
                            },
                            body: vec![ast::Statement::Return],
                            els: vec![],
                        }),
                        // If the spec has promised this is infallible, trust them
                        // We'll catch it later
                        Infallible => (),
                    }
                }

                ast::Expr::Var(result)
            }
            Expr::Assign(assign_expr) => {
                let val_expr = self.lower_expr(mode, &assign_expr.rhs);
                let ident =
                    user_ident(&self.scope.lookup(ScopedVarIndex::ParamVar(assign_expr.lhs)));
                self.stmts.push(ast::Statement::Assign {
                    var: ident.clone(),
                    val: val_expr,
                });
                ast::Expr::Var(ident)
            }
            Expr::Compare(comp_expr) => {
                let op = match comp_expr.kind {
                    CompareExprKind::Eq => ast::Op::EQ,
                    CompareExprKind::Neq => ast::Op::NEQ,
                    CompareExprKind::Lte => ast::Op::LTE,
                    CompareExprKind::Gte => ast::Op::GTE,
                    CompareExprKind::Lt => ast::Op::LT,
                    CompareExprKind::Gt => ast::Op::GT,
                };

                let left_expr = self.lower_expr(mode, &comp_expr.lhs);
                let right_expr = self.lower_expr(mode, &comp_expr.rhs);

                ast::Expr::BinOp {
                    op,
                    lhs: Box::new(left_expr),
                    rhs: Box::new(right_expr),
                }
            }
            Expr::Cast(cast_expr) => {
                //todo: cast exprs
                self.lower_expr(mode, &cast_expr.expr)
            }
        }
    }
}

fn fresh_ident(hint: &str) -> ast::Ident {
    static mut IDENT_CT: u32 = 0;
    let index = unsafe {
        IDENT_CT += 1;
        IDENT_CT
    };

    ast::Ident(format!("gen_{}_{}", index, hint))
}

fn lower_param_var(env: &Env, pv: &ParamVar) -> ast::TypedVar {
    ast::TypedVar {
        var: user_ident(&pv.ident),
        typ: type_ident(env, pv.type_),
    }
}

trait Scope {
    fn lookup(&self, var: ScopedVarIndex) -> String;
}

impl Scope for FnDef {
    fn lookup(&self, var: ScopedVarIndex) -> String {
        match var {
            ScopedVarIndex::ParamVar(pv) => self.sig.param_vars[pv].ident.to_string(),
            ScopedVarIndex::LocalVar(lv) => {
                self.body.as_ref().unwrap().local_vars[lv].ident.to_string()
            }
        }
    }
}

impl Scope for OpDef {
    fn lookup(&self, var: ScopedVarIndex) -> String {
        match var {
            ScopedVarIndex::ParamVar(pv) => self.sig.param_vars[pv].ident.to_string(),
            ScopedVarIndex::LocalVar(lv) => self.body.local_vars[lv].ident.to_string(),
        }
    }
}
