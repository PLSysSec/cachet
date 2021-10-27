use std::fmt::Display;

#[derive(Debug)]
pub struct Program(pub Vec<Decl>);

#[derive(Debug, Clone)]
pub struct Ident(pub String);

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug)]
pub enum Decl {
    Type {
        name: Ident,
        keyword: Option<String>,
    },
    Var(TypedVar),
    Const(TypedVar),
    Axiom(Expr),
    Proc {
        name: Ident,
        params: Vec<TypedVar>,
        returns: Vec<TypedVar>,
        modifies: Vec<Ident>,
        ensures: Vec<Ensure>,
        body: Option<Vec<Statement>>,
    },

    /// Represents an abstract function, we don't currently support
    /// defined functions since we don't need them yet
    Func {
        name: Ident,
        params: Vec<TypedVar>,
        return_type: Ident,
        keyword: Option<String>,
    },

    /// Sometimes it's just not worth encoding special cases
    Hack(String),
}

#[derive(Debug)]
pub struct Ensure {
    pub is_free: bool,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Statement {
    Var(TypedVar),
    ProcCall {
        returns: Vec<Ident>,
        proc: Ident,
        args: Vec<Expr>,
    },
    If {
        cond: Expr,
        body: Vec<Statement>,
        els: Vec<Statement>,
    },
    Assign {
        var: Ident,
        val: Expr,
    },
    Assert(Expr),
    Assume(Expr),
    Return,
}

#[derive(Debug)]
pub enum Expr {
    Neg(Box<Expr>),
    Call {
        func: Ident,
        args: Vec<Expr>,
    },
    BinOp {
        op: Op,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Var(Ident),
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum Op {
    PLUS,
    EQ,
    NEQ,
    LTE,
    GTE,
    LT,
    GT,
}

#[derive(Clone, Debug)]
pub struct TypedVar {
    pub var: Ident,
    pub typ: Ident,
}

#[derive(Debug)]
pub struct Procedure {
    name: Ident,
    returns: Ident,
    modifies: Vec<Ident>,
}
