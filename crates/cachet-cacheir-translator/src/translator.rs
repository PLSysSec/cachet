// vim: set tw=99 ts=4 sts=4 sw=4 et:

use lazy_static::lazy_static;

use cachet_lang::ast::*;
use cachet_lang::parser::*;

use crate::ast as stub;

lazy_static! {
    static ref BOOL_PATH: Path = Path::from_ident("Bool");
    static ref TRUE_PATH: Path = Path::from_ident("true");
    static ref FALSE_PATH: Path = Path::from_ident("false");
    static ref INT32_PATH: Path = Path::from_ident("Int32");
    static ref INT64_PATH: Path = Path::from_ident("Int64");
    static ref CACHE_IR_PATH: Path = Path::from_ident("CacheIR");
    static ref VALUE_PATH: Path = Path::from_ident("Value");
    static ref VALUE_TYPE_PATH: Path = Path::from_ident("ValueType");
    static ref OBJECT_PATH: Path = Path::from_ident("Object");
    static ref SHAPE_PATH: Path = Path::from_ident("Shape");
    static ref CLASS_PATH: Path = Path::from_ident("Class");
    static ref STRING_PATH: Path = Path::from_ident("String");
    static ref SYMBOL_PATH: Path = Path::from_ident("Symbol");
    static ref JSOP_PATH: Path = Path::from_ident("JSOp");
    static ref GUARD_CLASS_KIND_PATH: Path = Path::from_ident("GuardClassKind");
    static ref JS_WHY_MAGIC_PATH: Path = Path::from_ident("JSWhyMagic");
    static ref CALL_FLAGS_PATH: Path = Path::from_ident("CallFlags");
    static ref ARG_FORMAT_PATH: Path = Path::from_ident("ArgFormat");
    static ref SCALAR_TYPE_PATH: Path = Path::from_ident("ScalarType");
    static ref UNARY_MATH_FUNCTION_PATH: Path = Path::from_ident("UnaryMathFunction");
    static ref WASM_VAL_TYPE_PATH: Path = Path::from_ident("WasmValType");
    static ref JS_NATIVE_PATH: Path = Path::from_ident("JSNative");
    static ref ALLOC_KIND_PATH: Path = Path::from_ident("AllocKind");
    static ref VALUE_ID_PATH: Path = Path::from_ident("ValueId");
    static ref OBJECT_ID_PATH: Path = Path::from_ident("ObjectId");
    static ref STRING_ID_PATH: Path = Path::from_ident("StringId");
    static ref SYMBOL_ID_PATH: Path = Path::from_ident("SymbolId");
    static ref BOOL_ID_PATH: Path = Path::from_ident("BoolId");
    static ref INT32_ID_PATH: Path = Path::from_ident("Int32Id");
    static ref NUMBER_ID_PATH: Path = Path::from_ident("NumberId");
    static ref BIG_INT_ID_PATH: Path = Path::from_ident("BigIntId");
    static ref VALUE_TAG_ID_PATH: Path = Path::from_ident("ValueTagId");
    static ref INT_PTR_ID_PATH: Path = Path::from_ident("IntPtrId");
    static ref RAW_ID_PATH: Path = Path::from_ident("RawId");
    static ref INT32_FIELD_PATH: Path = Path::from_ident("Int32Field");
    static ref INT_PTR_FIELD_PATH: Path = Path::from_ident("IntPtrField");
    static ref SHAPE_FIELD_PATH: Path = Path::from_ident("ShapeField");
    static ref GETTER_SETTER_FIELD_PATH: Path = Path::from_ident("GetterSetterField");
    static ref OBJECT_FIELD_PATH: Path = Path::from_ident("ObjectField");
    static ref SYMBOL_FIELD_PATH: Path = Path::from_ident("SymbolField");
    static ref STRING_FIELD_PATH: Path = Path::from_ident("StringField");
    static ref BASE_SCRIPT_FIELD_PATH: Path = Path::from_ident("BaseScriptField");
    static ref ID_FIELD_PATH: Path = Path::from_ident("IdField");
    static ref ALLOC_SITE_FIELD_PATH: Path = Path::from_ident("AllocSiteField");
    static ref INT64_FIELD_PATH: Path = Path::from_ident("Int64Field");
    static ref VALUE_FIELD_PATH: Path = Path::from_ident("ValueField");
}

pub fn translate(stub: &stub::Stub) -> Mod {
    let params = Vec::new();

    let mut stmts = Vec::new();

    stmts.extend(stub.input_operands.iter().map(|input_operand| {
        Spanned::internal(
            Expr::Invoke(Call {
                target: Spanned::internal(Path::from_ident(format!(
                    "initInput{}",
                    type_path_for_operand_type(input_operand.data.type_)
                ))),
                args: Spanned::internal(vec![Spanned::internal(
                    generate_operand_id_from_id_call(
                        input_operand.data.id,
                        input_operand.data.type_,
                    )
                    .into(),
                )]),
            })
            .into(),
        )
    }));

    for field in &stub.fields {
        let data_expr = match field.data {
            stub::FieldData::Shape(shape) => Some(generate_from_addr_call(*SHAPE_PATH, shape)),
            stub::FieldData::RawInt32(n) => Some(Literal::Int32(n).into()),
            stub::FieldData::RawInt64(n) => Some(Literal::Int64(n).into()),
            _ => None,
        };

        if let Some(data_expr) = data_expr {
            let field_expr = generate_field_from_offset_call(field.offset, field.type_());

            let read_field_expr = Expr::Invoke(Call {
                target: Spanned::internal(
                    CACHE_IR_PATH.nest(format!("read{}", type_path_for_field_type(field.type_()))),
                ),
                args: Spanned::internal(vec![Spanned::internal(field_expr.into())]),
            });

            stmts.push(Spanned::internal(
                CheckStmt {
                    kind: CheckKind::Assume,
                    cond: Spanned::internal(
                        BinOperExpr {
                            oper: Spanned::internal(CompareBinOper::Eq.into()),
                            lhs: Spanned::internal(read_field_expr),
                            rhs: Spanned::internal(data_expr),
                        }
                        .into(),
                    ),
                }
                .into(),
            ));
        }
    }

    for fact in &stub.facts {
        match fact {
            stub::Fact::ShapeClass(fact) => {
                stmts.push(Spanned::internal(
                    CheckStmt {
                        kind: CheckKind::Assume,
                        cond: Spanned::internal(
                            BinOperExpr {
                                oper: Spanned::internal(CompareBinOper::Eq.into()),
                                lhs: Spanned::internal(Expr::Invoke(Call {
                                    target: Spanned::internal(SHAPE_PATH.nest("classOf")),
                                    args: Spanned::internal(vec![Spanned::internal(
                                        generate_from_addr_call(*SHAPE_PATH, fact.shape).into(),
                                    )]),
                                })),
                                rhs: Spanned::internal(generate_from_addr_call(
                                    *CLASS_PATH,
                                    fact.class,
                                )),
                            }
                            .into(),
                        ),
                    }
                    .into(),
                ));
            }
            stub::Fact::ShapeNumFixedSlots(fact) => {
                stmts.push(Spanned::internal(
                    CheckStmt {
                        kind: CheckKind::Assume,
                        cond: Spanned::internal(
                            BinOperExpr {
                                oper: Spanned::internal(CompareBinOper::Eq.into()),
                                lhs: Spanned::internal(Expr::Invoke(Call {
                                    target: Spanned::internal(SHAPE_PATH.nest("numFixedSlots")),
                                    args: Spanned::internal(vec![Spanned::internal(
                                        generate_from_addr_call(*SHAPE_PATH, fact.shape).into(),
                                    )]),
                                })),
                                rhs: Spanned::internal(
                                    Literal::UInt32(fact.num_fixed_slots).into(),
                                ),
                            }
                            .into(),
                        ),
                    }
                    .into(),
                ));
            }
            stub::Fact::ShapeSlotSpan(fact) => {
                stmts.push(Spanned::internal(
                    CheckStmt {
                        kind: CheckKind::Assume,
                        cond: Spanned::internal(
                            BinOperExpr {
                                oper: Spanned::internal(CompareBinOper::Eq.into()),
                                lhs: Spanned::internal(Expr::Invoke(Call {
                                    target: Spanned::internal(SHAPE_PATH.nest("slotSpan")),
                                    args: Spanned::internal(vec![Spanned::internal(
                                        generate_from_addr_call(*SHAPE_PATH, fact.shape).into(),
                                    )]),
                                })),
                                rhs: Spanned::internal(Literal::UInt32(fact.slot_span).into()),
                            }
                            .into(),
                        ),
                    }
                    .into(),
                ));
            }
            stub::Fact::ClassIsNativeObjectFact(fact) => {
                stmts.push(Spanned::internal(
                    CheckStmt {
                        kind: CheckKind::Assume,
                        cond: Spanned::internal(Expr::Invoke(Call {
                            target: Spanned::internal(CLASS_PATH.nest("isNativeObject")),
                            args: Spanned::internal(vec![Spanned::internal(
                                generate_from_addr_call(*CLASS_PATH, fact.class).into(),
                            )]),
                        })),
                    }
                    .into(),
                ));
            }
        }
    }

    stmts.extend(stub.ops.iter().map(|op| {
        Spanned::internal(Stmt::Emit(Call {
            target: Spanned::internal(CACHE_IR_PATH.nest(op.ident)),
            args: Spanned::internal(
                op.args
                    .iter()
                    .map(|arg| {
                        Spanned::internal(
                            match &arg.data {
                                stub::OpArgData::OperandId(data) => {
                                    generate_operand_id_from_id_call(data.id, data.type_)
                                }
                                stub::OpArgData::FieldOffset(data) => {
                                    generate_field_from_offset_call(data.offset, data.type_)
                                }
                                stub::OpArgData::ImmValue(data) => translate_imm_value(data),
                            }
                            .into(),
                        )
                    })
                    .collect(),
            ),
        }))
    }));

    let body = Block {
        stmts,
        value: Spanned::internal(None),
    };

    let op_item = CallableItem {
        ident: Spanned::internal(stub.kind),
        attrs: Vec::new(),
        is_unsafe: false,
        params,
        emits: None,
        ret: None,
        body: Spanned::internal(Some(body)),
    };

    let ir_item = IrItem {
        ident: Spanned::internal("CacheStub".into()),
        emits: Some(Spanned::internal(*CACHE_IR_PATH)),
        items: vec![Spanned::internal(Item::Op(op_item))],
    };

    Mod {
        items: vec![
            Spanned::internal(
                ImportItem {
                    file_path: Spanned::internal("../../../../../notes/cacheir.cachet".into()),
                }
                .into(),
            ),
            Spanned::internal(
                ImportItem {
                    file_path: Spanned::internal("../../../../../notes/utils.cachet".into()),
                }
                .into(),
            ),
            Spanned::internal(ir_item.into()),
        ],
    }
}

fn translate_imm_value(imm_value: &stub::ImmValue) -> Expr {
    match imm_value {
        stub::ImmValue::JSOp(ident) => Spanned::internal(JSOP_PATH.nest(*ident)).into(),
        stub::ImmValue::Bool(b) => translate_bool(*b),
        stub::ImmValue::Byte(n) => Literal::UInt8(*n).into(),
        stub::ImmValue::GuardClassKind(ident) => {
            Spanned::internal(GUARD_CLASS_KIND_PATH.nest(*ident)).into()
        }
        stub::ImmValue::ValueType(ident) => Spanned::internal(VALUE_TYPE_PATH.nest(*ident)).into(),
        stub::ImmValue::JSWhyMagic(ident) => {
            Spanned::internal(JS_WHY_MAGIC_PATH.nest(*ident)).into()
        }
        stub::ImmValue::CallFlags(call_flags) => Expr::Invoke(Call {
            target: Spanned::internal(CALL_FLAGS_PATH.nest("new")),
            args: Spanned::internal(vec![
                Spanned::internal(
                    Expr::from(Spanned::internal(
                        ARG_FORMAT_PATH.nest(call_flags.arg_format),
                    ))
                    .into(),
                ),
                Spanned::internal(translate_bool(call_flags.is_constructing).into()),
                Spanned::internal(translate_bool(call_flags.is_same_realm).into()),
                Spanned::internal(translate_bool(call_flags.needs_uninitialized_this).into()),
            ]),
        }),
        stub::ImmValue::ScalarType(ident) => {
            Spanned::internal(SCALAR_TYPE_PATH.nest(*ident)).into()
        }
        stub::ImmValue::UnaryMathFunction(ident) => {
            Spanned::internal(UNARY_MATH_FUNCTION_PATH.nest(*ident)).into()
        }
        stub::ImmValue::WasmValType(ident) => {
            Spanned::internal(WASM_VAL_TYPE_PATH.nest(*ident)).into()
        }
        stub::ImmValue::Int32(n) => Literal::Int32(*n).into(),
        stub::ImmValue::UInt32(n) => Literal::UInt32(*n).into(),
        stub::ImmValue::JSNative(addr) => generate_from_addr_call(*JS_NATIVE_PATH, *addr),
        stub::ImmValue::AllocKind(ident) => Spanned::internal(ALLOC_KIND_PATH.nest(*ident)).into(),
    }
}

fn translate_bool(b: bool) -> Expr {
    Spanned::internal(if b { *TRUE_PATH } else { *FALSE_PATH }).into()
}

fn generate_operand_id_from_id_call(id: u16, type_: stub::OperandType) -> Expr {
    Expr::Invoke(Call {
        target: Spanned::internal(type_path_for_operand_type(type_).nest("fromId")),
        args: Spanned::internal(vec![Spanned::internal(
            Expr::from(Literal::UInt16(id)).into(),
        )]),
    })
}

fn generate_field_from_offset_call(offset: u32, type_: stub::FieldType) -> Expr {
    Expr::Invoke(Call {
        target: Spanned::internal(type_path_for_field_type(type_).nest("fromOffset")),
        args: Spanned::internal(vec![Spanned::internal(
            Expr::from(Literal::UInt32(offset)).into(),
        )]),
    })
}

fn generate_from_addr_call(type_path: Path, addr: stub::Addr) -> Expr {
    Expr::Invoke(Call {
        target: Spanned::internal(type_path.nest("fromAddr")),
        args: Spanned::internal(vec![Spanned::internal(
            Expr::from(Literal::UInt64(addr)).into(),
        )]),
    })
}

fn type_path_for_operand_type(operand_type: stub::OperandType) -> Path {
    match operand_type {
        stub::OperandType::Val => *VALUE_ID_PATH,
        stub::OperandType::Obj => *OBJECT_ID_PATH,
        stub::OperandType::String => *STRING_ID_PATH,
        stub::OperandType::Symbol => *SYMBOL_ID_PATH,
        stub::OperandType::Boolean => *BOOL_ID_PATH,
        stub::OperandType::Int32 => *INT32_ID_PATH,
        stub::OperandType::Number => *NUMBER_ID_PATH,
        stub::OperandType::BigInt => *BIG_INT_ID_PATH,
        stub::OperandType::ValueTag => *VALUE_TAG_ID_PATH,
        stub::OperandType::IntPtr => *INT_PTR_ID_PATH,
        stub::OperandType::Raw => *RAW_ID_PATH,
    }
}

fn type_path_for_field_type(field_type: stub::FieldType) -> Path {
    match field_type {
        stub::FieldType::Shape => *SHAPE_FIELD_PATH,
        stub::FieldType::GetterSetter => *GETTER_SETTER_FIELD_PATH,
        stub::FieldType::Object => *OBJECT_FIELD_PATH,
        stub::FieldType::String => *STRING_FIELD_PATH,
        stub::FieldType::Atom => type_path_for_field_type(stub::FieldType::String),
        stub::FieldType::PropertyName => type_path_for_field_type(stub::FieldType::String),
        stub::FieldType::Symbol => *SYMBOL_FIELD_PATH,
        stub::FieldType::BaseScript => *BASE_SCRIPT_FIELD_PATH,
        stub::FieldType::RawInt32 => *INT32_FIELD_PATH,
        stub::FieldType::RawPointer => *INT_PTR_FIELD_PATH,
        stub::FieldType::Id => *ID_FIELD_PATH,
        stub::FieldType::Value => *VALUE_FIELD_PATH,
        stub::FieldType::RawInt64 => *INT64_FIELD_PATH,
        stub::FieldType::AllocSite => *ALLOC_SITE_FIELD_PATH,
    }
}
