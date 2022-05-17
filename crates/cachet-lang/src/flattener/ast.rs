// vim: set tw=99 ts=4 sts=4 sw=4 et:

use void::Void;

use crate::normalizer;
pub use crate::normalizer::{
    BindStmt, CallableIndex, DeclIndex, EnumIndex, EnumItem, EnumVariantIndex, Field, FieldIndex,
    FnIndex, FreeVarRef, GlobalVarIndex, GlobalVarItem, GotoStmt, HasAttrs, IrIndex, IrItem,
    Label, LabelArg, LabelIndex, LabelParam, LabelParamIndex, LabelStmt, Literal, LocalCallIndex,
    LocalLabelIndex, LocalVar, LocalVarIndex, Locals, NotPartOfDeclOrderError, OpIndex,
    ParamIndex, Params, ParentIndex, StructFieldIndex, StructIndex, StructItem, TypeIndex, Typed,
    VarExpr, VarIndex, VarParam, VarParamIndex, VarRef, VarRefArg, VariantIndex,
};

pub type Env = normalizer::Env<Void>;
pub type CallableItem = normalizer::CallableItem<Void>;
pub type Arg = normalizer::Arg<Void>;
pub type Call = normalizer::Call<Void>;
pub type Body = normalizer::Body<Void>;

pub type Stmt = normalizer::Stmt<Void>;
pub type LetStmt = normalizer::LetStmt<Void>;
pub type IfStmt = normalizer::IfStmt<Void>;
pub type ElseClause = normalizer::ElseClause<Void>;
pub type CheckStmt = normalizer::CheckStmt<Void>;
pub type EmitStmt = normalizer::EmitStmt<Void>;
pub type InvokeStmt = normalizer::InvokeStmt<Void>;
pub type AssignStmt = normalizer::AssignStmt<Void>;
pub type RetStmt = normalizer::RetStmt<Void>;

pub type Expr = normalizer::Expr<Void>;
pub type PureExpr = normalizer::PureExpr<Void>;
pub type InvokeExpr = normalizer::InvokeExpr<Void>;
pub type FieldAccessExpr<E = Expr> = normalizer::FieldAccessExpr<E>;
pub type NegateExpr<E = Expr> = normalizer::NegateExpr<E>;
pub type CastExpr<E = Expr> = normalizer::CastExpr<E>;
pub type BinOperExpr = normalizer::BinOperExpr<Void>;
