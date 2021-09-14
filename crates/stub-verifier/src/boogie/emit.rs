use std::fmt::{Display, Write};

use crate::boogie::ast::*;
use indent_write::fmt::IndentWriter;

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Sep(&self.0, "\n"))
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::Type { name, keyword } => {
                write!(f, "type {} {};", Keyword(keyword), name)
            }
            Decl::Var(tv) => {
                write!(f, "var {}", tv)
            }
            Decl::Const(tv) => {
                write!(f, "const {}", tv)
            }
            Decl::Axiom(_) => todo!(),
            Decl::Proc {
                name,
                params,
                returns,
                modifies,
                ensures,
                body,
            } => {
                write!(f, "procedure {}({})", name, Sep(params, ", "))?;

                if !returns.is_empty() {
                    write!(f, "returns ({})", Sep(returns, ", "))?;
                }

                if !modifies.is_empty() {
                    write!(f, " modifies {};", Sep(modifies, ", "))?;
                }

                if !ensures.is_empty() {
                    write!(f, "{};", Sep(ensures, "; "))?;
                }

                if ensures.is_empty() && modifies.is_empty() && body.is_none() {
                    f.write_str(";")?;
                }

                if let Some(b) = body {
                    write!(f, "\n{{\n{}\n}}", Indent(Sep(b, "\n")))?;
                }

                Ok(())
            }
            Decl::Func {
                name,
                params,
                return_type,
                keyword,
            } => {
                write!(
                    f,
                    "function {} {}({}): {};",
                    Keyword(keyword),
                    name,
                    Sep(params, ", "),
                    return_type
                )
            }
            Decl::Hack(s) => f.write_str(s),
        }
    }
}

impl Display for Ensure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_free {
            write!(f, "free ensures {}", self.expr)
        } else {
            write!(f, "ensures {}", self.expr)
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Var(tv) => {
                write!(f, "var {};", tv)
            }
            Statement::If { cond, body, els } => {
                write!(
                    f,
                    "if ({}) {{\n{}\n}} else {{\n{}\n}}",
                    cond,
                    Indent(Sep(body, "\n")),
                    Indent(Sep(els, "\n"))
                )
            }
            Statement::ProcCall {
                returns,
                proc,
                args,
            } => {
                write!(
                    f,
                    "call {} := {}({});",
                    Sep(returns, ", "),
                    proc,
                    Sep(args, ", ")
                )
            }
            Statement::Assign { var, val } => {
                write!(f, "{} := {};", var, val)
            }
            Statement::Assert(e) => {
                write!(f, "assert {};", e)
            }
            Statement::Assume(e) => {
                write!(f, "assume {};", e)
            }
            Statement::Return => f.write_str("return;"),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Neg(e) => write!(f, "!({})", e),
            Expr::Call { func, args } => write!(f, "{}({})", func, Sep(args, ", ")),
            Expr::BinOp { op, lhs, rhs } => {
                let op_txt = match op {
                    Op::PLUS => "+",
                    Op::EQ => "==",
                    Op::NEQ => "!=",
                    Op::LTE => "<=",
                    Op::GTE => ">=",
                    Op::LT => "<",
                    Op::GT => ">",
                };

                write!(f, "({} {} {})", lhs, op_txt, rhs)
            }
            Expr::Var(i) => write!(f, "{}", i),
        }
    }
}

impl Display for TypedVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.var, self.typ)
    }
}

struct Keyword<'a>(&'a Option<String>);
impl Display for Keyword<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Some(kw) => write!(f, "{{:{}}}", kw),
            None => Ok(()),
        }
    }
}

struct Indent<D: Display>(D);
impl<D: Display> Display for Indent<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(IndentWriter::new("\t", f), "{}", self.0)
    }
}

struct Optional<D: Display>(Option<D>);
impl<D: Display> Display for Optional<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            Some(b) => b.fmt(f),
            None => Ok(()),
        }
    }
}

struct Sep<'a, D, I>(&'a I, &'static str)
where
    D: Display,
    &'a I: IntoIterator<Item = D>;

impl<'a, D, I> Display for Sep<'a, D, I>
where
    D: Display,
    &'a I: IntoIterator<Item = D>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for i in self.0.into_iter() {
            if !first {
                f.write_str(self.1)?;
            }
            first = false;

            write!(f, "{}", i)?;
        }
        Ok(())
    }
}
