// vim: set tw=99 ts=4 sts=4 sw=4 et:

use void::Void;

use crate::normalizer;
pub use crate::normalizer::{
    Arg, BinOperExpr, BindStmt, Call, CallableIndex, DeclIndex, EmitStmt, EnumIndex, EnumItem,
    EnumVariantIndex, Field, FieldIndex, FnIndex, GlobalVarIndex, HasAttrs, HasIr, InvokeExpr,
    InvokeStmt, IrIndex, IrItem, Label, LabelIndex, LabelParam, LabelParamIndex, LabelStmt,
    Literal, LocalLabelIndex, LocalVar, LocalVarIndex, Locals, NotPartOfDeclOrderError, OpIndex,
    OutLabelArg, OutVarArg, ParamIndex, Params, ParentIndex, PlainLabelExpr, PureExpr,
    PureLabelExpr, StructFieldIndex, StructIndex, StructItem, TypeIndex, Typed, VarExpr, VarField,
    VarIndex, VarParam, VarParamIndex, VariantIndex,
};

pub type Env = normalizer::Env<Void>;
pub type CallableItem = normalizer::CallableItem<Void>;
pub type GlobalVarItem = normalizer::GlobalVarItem<Void>;
pub type Body = normalizer::Body<Void>;

pub type Stmt = normalizer::Stmt<Void>;
pub type LetStmt = normalizer::LetStmt<Void>;
pub type IfStmt = normalizer::IfStmt<Void>;
pub type ForInStmt = normalizer::ForInStmt<Void>;
pub type WhileStmt = normalizer::WhileStmt<Void>;
pub type ElseClause = normalizer::ElseClause<Void>;
pub type CheckStmt = normalizer::CheckStmt<Void>;
pub type GotoStmt = normalizer::GotoStmt<Void>;
pub type AssignStmt = normalizer::AssignStmt<Void>;
pub type RetStmt = normalizer::RetStmt<Void>;

pub type Expr = normalizer::Expr<Void>;
pub type FieldAccess<E = Expr> = normalizer::FieldAccess<E>;
pub type FieldAccessExpr<E = Expr> = normalizer::FieldAccessExpr<E>;
pub type FieldAccessLabelExpr<E = Expr> = normalizer::FieldAccessLabelExpr<E>;
pub type NegateExpr<E = Expr> = normalizer::NegateExpr<E>;
pub type CastExpr<E = Expr> = normalizer::CastExpr<E>;

pub type LabelExpr<E = Expr> = normalizer::LabelExpr<E>;
