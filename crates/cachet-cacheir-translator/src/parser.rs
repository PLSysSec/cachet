// vim: set tw=99 ts=4 sts=4 sw=4 et:

/*
use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar, "/parser/grammar.rs");
use lazy_static::lazy_static;
*/

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric1, hex_digit1, multispace0};
//use nom::character::complete::char;
use nom::combinator::{complete, map, recognize, value};
use nom::error::{ErrorKind as ParseErrorKind, ParseError as ParseErrorTrait};
use nom::multi::{many0, many0_count, many_till, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::Err as ParseErr;
//use nom::InputTakeAtPosition;
use strum::IntoEnumIterator;

use crate::ast::*;

pub fn parse(input: &str) -> Result<Stub, ParseError> {
    match complete(preceded(multispace0, parse_stub))(input) {
        Ok((_, stub)) => Ok(stub),
        Err(ParseErr::Error(err) | ParseErr::Failure(err)) => Err(err),
        Err(ParseErr::Incomplete(_)) => unreachable!("incomplete parse"),
    }
}

type ParseError<'a> = nom::error::VerboseError<&'a str>;
type ParseResult<'a, O> = nom::IResult<&'a str, O, ParseError<'a>>;
trait Parser<'a, O> = nom::Parser<&'a str, O, ParseError<'a>>;

fn parse_stub(input: &str) -> ParseResult<Stub> {
    let (input, kind) = parse_ident(input)?;
    let (input, engine) = parse_engine(input)?;
    let (input, (input_operands, _)) = many_till(parse_input_operand, terminator)(input)?;
    let (input, (ops, _)) = many_till(parse_op, terminator)(input)?;
    let (input, (fields, _)) = many_till(parse_field, terminator)(input)?;
    let (input, facts) = many0(parse_fact)(input)?;
    Ok((
        input,
        Stub {
            kind,
            engine,
            input_operands,
            ops,
            fields,
            facts,
        },
    ))
}

fn parse_engine(input: &str) -> ParseResult<Engine> {
    alt((
        value(Engine::Baseline, symbol("Baseline")),
        value(Engine::IonIC, symbol("IonIC")),
    ))(input)
}

fn parse_input_operand(input: &str) -> ParseResult<InputOperand> {
    preceded(symbol("Input"), |input| {
        parse_arg(input, parse_operand_type, |type_| {
            move |input| parse_operand_id(input, type_)
        })
    })(input)
}

fn parse_op(input: &str) -> ParseResult<Op> {
    let (input, ident) = parse_ident(input)?;
    let (input, args) = parse_op_args(input)?;
    Ok((input, Op { ident, args }))
}

fn parse_op_args(input: &str) -> ParseResult<Vec<OpArg>> {
    separated_list0(comma, parse_op_arg)(input)
}

fn parse_arg<'a, T1, T2, P1: Parser<'a, T1>, P2: Parser<'a, T2>>(
    input: &'a str,
    parse_arg_type: P1,
    parse_arg_data: impl FnOnce(T1) -> P2,
) -> ParseResult<Arg<T2>> {
    let (input, (type_, ident)) =
        delimited(symbol("["), pair(parse_arg_type, parse_ident), symbol("]"))(input)?;
    let (input, data) = parse_arg_data(type_).parse(input)?;
    Ok((input, Arg { ident, data }))
}

fn parse_op_arg(input: &str) -> ParseResult<OpArg> {
    parse_arg(input, parse_op_arg_type, |type_| {
        move |input| parse_op_arg_data(input, type_)
    })
}

fn parse_op_arg_type(input: &str) -> ParseResult<OpArgType> {
    alt((
        map(parse_operand_type, Into::into),
        map(parse_field_type, Into::into),
        map(parse_imm_type, Into::into),
    ))(input)
}

fn parse_op_arg_data(input: &str, type_: OpArgType) -> ParseResult<OpArgData> {
    match type_ {
        OpArgType::OperandId(operand_type) => {
            map(|input| parse_operand_id(input, operand_type), Into::into)(input)
        }
        OpArgType::FieldOffset(field_type) => {
            map(|input| parse_field_offset(input, field_type), Into::into)(input)
        }
        OpArgType::ImmValue(imm_type) => {
            map(|input| parse_imm_value(input, imm_type), Into::into)(input)
        }
    }
}

fn parse_operand_id(input: &str, type_: OperandType) -> ParseResult<OperandId> {
    map(u16, |index| OperandId { id: index, type_ }.into())(input)
}

fn parse_field_offset(input: &str, type_: FieldType) -> ParseResult<FieldOffset> {
    map(u32, |offset| FieldOffset { offset, type_ }.into())(input)
}

fn parse_imm_value(input: &str, type_: ImmType) -> ParseResult<ImmValue> {
    match type_ {
        ImmType::JSOp => map(parse_ident, ImmValue::JSOp)(input),
        ImmType::Bool => map(
            alt((value(true, symbol("true")), value(false, symbol("false")))),
            ImmValue::Bool,
        )(input),
        ImmType::Byte => map(u8, ImmValue::Byte)(input),
        ImmType::GuardClassKind => map(parse_ident, ImmValue::GuardClassKind)(input),
        ImmType::ValueType => map(parse_value_type, ImmValue::ValueType)(input),
        ImmType::JSWhyMagic => map(parse_ident, ImmValue::JSWhyMagic)(input),
        ImmType::CallFlags => map(parse_call_flags, ImmValue::CallFlags)(input),
        ImmType::ScalarType => map(parse_ident, ImmValue::ScalarType)(input),
        ImmType::UnaryMathFunction => map(parse_ident, ImmValue::UnaryMathFunction)(input),
        ImmType::WasmValType => map(parse_ident, ImmValue::WasmValType)(input),
        ImmType::Int32 => map(i32, ImmValue::Int32)(input),
        ImmType::UInt32 => map(u32, ImmValue::UInt32)(input),
        ImmType::JSNative => map(addr, ImmValue::JSNative)(input),
        /*
        ImmType::StaticString => map(
            delimited(
                char('"'),
                |input: &str| input.split_at_position_complete(|c| c == '"'),
                char('"'),
            ),
            |s| ImmValue::StaticString(s.to_owned()),
        )(input),
        */
        ImmType::AllocKind => map(parse_ident, ImmValue::AllocKind)(input),
    }
}

fn parse_value_type(input: &str) -> ParseResult<ValueType> {
    lexeme(alt_enum(enum_tag::<ValueType>))(input)
}

fn parse_call_flags(input: &str) -> ParseResult<CallFlags> {
    let (input, arg_format) = parse_ident(input)?;

    let mut call_flags = CallFlags {
        arg_format,
        is_constructing: false,
        is_same_realm: false,
        needs_uninitialized_this: false,
    };
    let (input, _) = many0_count(preceded(
        symbol("+"),
        alt((
            map(symbol("isConstructing"), |_| {
                call_flags.is_constructing = true;
                ()
            }),
            map(symbol("isSameRealm"), |_| {
                call_flags.is_same_realm = true;
                ()
            }),
            map(symbol("needsUninitializedThis"), |_| {
                call_flags.needs_uninitialized_this = true;
                ()
            }),
        )),
    ))(input)?;

    Ok((input, call_flags))
}

fn parse_operand_type(input: &str) -> ParseResult<OperandType> {
    terminated(alt_enum(enum_tag::<OperandType>), symbol("Id"))(input)
}

fn parse_field_type(input: &str) -> ParseResult<FieldType> {
    terminated(alt_enum(enum_tag::<FieldType>), symbol("Field"))(input)
}

fn parse_imm_type(input: &str) -> ParseResult<ImmType> {
    terminated(alt_enum(enum_tag::<ImmType>), symbol("Imm"))(input)
}

fn parse_field(input: &str) -> ParseResult<Field> {
    let (input, type_) = parse_field_type(input)?;
    let (input, offset) = terminated(u32, comma)(input)?;
    let (input, data) = parse_field_data(input, type_)?;
    Ok((input, Field { offset, data }))
}

fn parse_field_data(input: &str, type_: FieldType) -> ParseResult<FieldData> {
    match type_ {
        FieldType::Shape => map(addr, FieldData::Shape)(input),
        FieldType::GetterSetter => map(addr, FieldData::GetterSetter)(input),
        FieldType::Object => map(addr, FieldData::Object)(input),
        FieldType::String => map(addr, FieldData::String)(input),
        FieldType::Atom => map(addr, FieldData::Atom)(input),
        FieldType::PropertyName => map(hex_word, FieldData::Atom)(input),
        FieldType::Symbol => map(addr, FieldData::Symbol)(input),
        FieldType::BaseScript => map(addr, FieldData::BaseScript)(input),
        FieldType::RawInt32 => map(i32, FieldData::RawInt32)(input),
        FieldType::RawPointer => map(addr, FieldData::RawPointer)(input),
        FieldType::Id => map(hex_word, FieldData::Id)(input),
        FieldType::Value => map(hex_u64, FieldData::Value)(input),
        FieldType::RawInt64 => map(i64, FieldData::RawInt64)(input),
        FieldType::AllocSite => map(addr, FieldData::AllocSite)(input),
    }
}

fn parse_fact(input: &str) -> ParseResult<Fact> {
    alt((
        map(parse_shape_class_fact, Into::into),
        map(parse_shape_num_fixed_slots_fact, Into::into),
        map(parse_shape_slot_span_fact, Into::into),
        map(parse_class_is_native_object_fact, Into::into),
    ))(input)
}

fn parse_shape_class_fact(input: &str) -> ParseResult<ShapeClassFact> {
    let (input, _) = symbol("ShapeClass")(input)?;
    let (input, shape) = terminated(addr, comma)(input)?;
    let (input, class) = addr(input)?;
    Ok((input, ShapeClassFact { shape, class }))
}

fn parse_shape_num_fixed_slots_fact(input: &str) -> ParseResult<ShapeNumFixedSlotsFact> {
    let (input, _) = symbol("ShapeNumFixedSlots")(input)?;
    let (input, shape) = terminated(addr, comma)(input)?;
    let (input, num_fixed_slots) = u32(input)?;
    Ok((
        input,
        ShapeNumFixedSlotsFact {
            shape,
            num_fixed_slots,
        },
    ))
}

fn parse_shape_slot_span_fact(input: &str) -> ParseResult<ShapeSlotSpanFact> {
    let (input, _) = symbol("ShapeSlotSpan")(input)?;
    let (input, shape) = terminated(addr, comma)(input)?;
    let (input, slot_span) = u32(input)?;
    Ok((input, ShapeSlotSpanFact { shape, slot_span }))
}

fn parse_class_is_native_object_fact(input: &str) -> ParseResult<ClassIsNativeObjectFact> {
    let (input, _) = symbol("ClassIsNativeObject")(input)?;
    let (input, class) = addr(input)?;
    Ok((input, ClassIsNativeObjectFact { class }))
}

fn parse_ident(input: &str) -> ParseResult<Ident> {
    lexeme(map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        Ident::from,
    ))(input)
}

fn u8(input: &str) -> ParseResult<u8> {
    lexeme(nom::character::complete::u8)(input)
}

fn u16(input: &str) -> ParseResult<u16> {
    lexeme(nom::character::complete::u16)(input)
}

fn i32(input: &str) -> ParseResult<i32> {
    lexeme(nom::character::complete::i32)(input)
}

fn u32(input: &str) -> ParseResult<u32> {
    lexeme(nom::character::complete::u32)(input)
}

fn i64(input: &str) -> ParseResult<i64> {
    lexeme(nom::character::complete::i64)(input)
}

fn hex_u64(input: &str) -> ParseResult<u64> {
    let (input, addr_str) = lexeme(hex_digit1)(input)?;
    Ok((input, u64::from_str_radix(addr_str, 16).unwrap()))
}

fn hex_word(input: &str) -> ParseResult<Word> {
    hex_u64(input)
}

fn addr(input: &str) -> ParseResult<Addr> {
    hex_word(input)
}

fn comma(input: &str) -> ParseResult<&'_ str> {
    symbol(",")(input)
}

fn terminator(input: &str) -> ParseResult<&'_ str> {
    symbol("%")(input)
}

fn lexeme<'a, O>(inner: impl Parser<'a, O>) -> impl FnMut(&'a str) -> ParseResult<'a, O> {
    terminated(inner, multispace0)
}

fn symbol<'a>(x: &'static str) -> impl FnMut(&'a str) -> ParseResult<&'a str> {
    lexeme(tag(x))
}

fn alt_enum<'a, T: IntoEnumIterator, O, P: Parser<'a, O>>(
    mut f: impl FnMut(T) -> P,
) -> impl Parser<'a, O> {
    move |input: &'a str| {
        let mut accum_err: Option<ParseError> = None;
        for x in T::iter() {
            match f(x).parse(input) {
                Err(ParseErr::Error(err)) => {
                    accum_err = match accum_err {
                        Some(prev_err) => Some(prev_err.or(err)),
                        None => Some(err),
                    };
                }
                res => return res,
            }
        }
        Err(ParseErr::Error(accum_err.map_or_else(
            || ParseError::from_error_kind(input, ParseErrorKind::Alt),
            |err| ParseError::append(input, ParseErrorKind::Alt, err),
        )))
    }
}

fn enum_tag<'a, T: Copy + Into<&'static str>>(x: T) -> impl Parser<'a, T> {
    value(x, tag(x.into()))
}
