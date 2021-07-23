// vim: set tw=99 ts=4 sts=4 sw=4 et:

mod ast;

use std::fmt::{self, Write};

use indent_write::fmt::IndentWriter;

use crate::util::{fmt_join, fmt_join_trailing};

pub use crate::backend::cpp::ast::*;

impl<Ident: fmt::Display> fmt::Display for Code<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt_join(f, "\n\n", self.defs.iter())
    }
}

impl<Ident: fmt::Display> fmt::Display for Def<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Def::Comment(comment_def) => fmt::Display::fmt(comment_def, f),
            Def::Namespace(namespace_def) => fmt::Display::fmt(namespace_def, f),
            Def::Fn(fn_def) => fmt::Display::fmt(fn_def, f),
        }
    }
}

impl fmt::Display for CommentDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(IndentWriter::new("// ", f), "{}", self.text)
    }
}

impl<Ident: fmt::Display> fmt::Display for NamespaceDef<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "namespace {} {{\n\n", self.ident)?;
        fmt_join_trailing(f, "\n\n", self.defs.iter())?;
        write!(f, "}};  // namespace {}", self.ident)
    }
}

impl<Ident: fmt::Display> fmt::Display for FnDef<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.is_inline {
            write!(f, "inline ")?;
        }
        write!(f, "{} {}(", self.ret, self.path)?;
        fmt_join(f, ", ", self.params.iter())?;
        write!(f, ")")?;
        match &self.body {
            None => write!(f, ";"),
            Some(body) => write!(f, " {}", body),
        }
    }
}

impl<Ident: fmt::Display> fmt::Display for Param<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} {}", self.type_, self.ident)
    }
}

impl<Ident: fmt::Display> fmt::Display for Type<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Type::Void => write!(f, "void"),
            Type::Path(path) => fmt::Display::fmt(path, f),
            Type::Template(template_type) => fmt::Display::fmt(template_type, f),
            Type::Const(const_type) => fmt::Display::fmt(const_type, f),
            Type::Ref(ref_type) => fmt::Display::fmt(ref_type, f),
        }
    }
}

impl<Ident: fmt::Display> fmt::Display for TemplateType<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}<", self.inner)?;
        fmt_join(f, ", ", self.args.iter())?;
        write!(f, ">")
    }
}

impl<Ident: fmt::Display> fmt::Display for ConstType<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} const", self.inner)
    }
}

impl<Ident: fmt::Display> fmt::Display for RefType<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}{}",
            self.inner,
            match self.value_category {
                ValueCategory::LValue => "&",
                ValueCategory::RValue => "&&",
            }
        )
    }
}

impl<Ident: fmt::Display> fmt::Display for Block<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{{\n")?;

        let mut indent_writer = IndentWriter::new("  ", f);
        fmt_join_trailing(&mut indent_writer, "\n", self.stmts.iter())?;
        let f = indent_writer.into_inner();

        write!(f, "}}")
    }
}

impl<Ident: fmt::Display> fmt::Display for Stmt<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Stmt::Expr(expr_stmt) => fmt::Display::fmt(expr_stmt, f),
            Stmt::Ret(ret_stmt) => fmt::Display::fmt(ret_stmt, f),
            Stmt::Var(var_stmt) => fmt::Display::fmt(var_stmt, f),
            Stmt::If(if_stmt) => fmt::Display::fmt(if_stmt, f),
        }
    }
}

impl<Ident: fmt::Display> fmt::Display for ExprStmt<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{};", self.expr)
    }
}

impl<Ident: fmt::Display> fmt::Display for RetStmt<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "return {};", self.value)
    }
}

impl<Ident: fmt::Display> fmt::Display for VarStmt<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} {}", self.type_, self.ident)?;
        if let Some(init) = &self.init {
            write!(f, "({})", init)?;
        }
        write!(f, ";")
    }
}

impl<Ident: fmt::Display> fmt::Display for IfStmt<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "if ({}) {}", self.cond, self.body)
    }
}

impl<Ident: fmt::Display> fmt::Display for Expr<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Expr::Block(block_expr) => fmt::Display::fmt(block_expr, f),
            Expr::Path(path) => fmt::Display::fmt(path, f),
            Expr::Template(template_expr) => fmt::Display::fmt(template_expr, f),
            Expr::Member(member_expr) => fmt::Display::fmt(member_expr, f),
            Expr::Call(call_expr) => fmt::Display::fmt(call_expr, f),
            Expr::Cast(cast_expr) => fmt::Display::fmt(cast_expr, f),
            Expr::Unary(unary_expr) => fmt::Display::fmt(unary_expr, f),
            Expr::Compare(compare_expr) => fmt::Display::fmt(compare_expr, f),
            Expr::Assign(assign_expr) => fmt::Display::fmt(assign_expr, f),
            Expr::Comma(comma_expr) => fmt::Display::fmt(comma_expr, f),
        }
    }
}

struct MaybeGrouped<'a, Ident = String>(&'a Expr<Ident>);

impl<Ident: fmt::Display> fmt::Display for MaybeGrouped<'_, Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let needs_group = match self.0 {
            // Note: BlockExpr and CommaExpr are always inherently grouped for
            // C++ syntax reasons, so there's no need for us to group them again
            // here.
            Expr::Block(_)
            | Expr::Path(_)
            | Expr::Template(_)
            | Expr::Member(_)
            | Expr::Call(_)
            | Expr::Unary(_)
            | Expr::Comma(_) => false,
            Expr::Compare(_) | Expr::Assign(_) => true,
            Expr::Cast(cast_expr) => match cast_expr.kind {
                CastExprKind::Functional(_) => false,
                CastExprKind::CStyle => true,
            },
        };
        if needs_group {
            write!(f, "({})", self.0)
        } else {
            fmt::Display::fmt(self.0, f)
        }
    }
}

impl<Ident: fmt::Display> fmt::Display for BlockExpr<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "({})", self.block)
    }
}

impl<Ident: fmt::Display> fmt::Display for TemplateExpr<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}<", self.inner)?;
        fmt_join(f, ", ", self.args.iter())?;
        write!(f, ">")
    }
}

impl<Ident: fmt::Display> fmt::Display for MemberExpr<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}.{}", MaybeGrouped(&self.parent), self.member)
    }
}

impl<Ident: fmt::Display> fmt::Display for CallExpr<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}(", MaybeGrouped(&self.target))?;
        fmt_join(f, ", ", self.args.iter())?;
        write!(f, ")")
    }
}

impl<Ident: fmt::Display> fmt::Display for CastExpr<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            CastExprKind::Functional(kind) => write!(f, "{}<{}>({})", kind, self.type_, self.expr),
            CastExprKind::CStyle => write!(f, "({}) {}", self.type_, MaybeGrouped(&self.expr)),
        }
    }
}

impl fmt::Display for FunctionalCastExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                FunctionalCastExprKind::Static => "static_cast",
                FunctionalCastExprKind::Dynamic => "dynamic_cast",
                FunctionalCastExprKind::Const => "const_cast",
                FunctionalCastExprKind::Reinterpret => "reinterpret_cast",
            }
        )
    }
}

impl<Ident: fmt::Display> fmt::Display for UnaryExpr<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}{}", self.kind, MaybeGrouped(&self.inner))
    }
}

impl fmt::Display for UnaryExprKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                UnaryExprKind::BoolNot => "!",
            }
        )
    }
}

impl<Ident: fmt::Display> fmt::Display for CompareExpr<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{} {} {}",
            MaybeGrouped(&self.lhs),
            self.kind,
            MaybeGrouped(&self.rhs)
        )
    }
}

impl<Ident: fmt::Display> fmt::Display for AssignExpr<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{} = {}",
            MaybeGrouped(&self.lhs),
            MaybeGrouped(&self.rhs)
        )
    }
}

impl<Ident: fmt::Display> fmt::Display for CommaExpr<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "({}, {})",
            MaybeGrouped(&self.lhs),
            MaybeGrouped(&self.rhs)
        )
    }
}
