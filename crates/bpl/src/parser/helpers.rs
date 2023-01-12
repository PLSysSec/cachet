// vim: set tw=99 ts=4 sts=4 sw=4 et:

use crate::ast::{BvLit, Dec, Exp, Float, FloatValue, PlainFloatValue, Spanned};

pub type RawParseError<T> = lalrpop_util::ParseError<usize, T, UserParseError>;
pub type UserParseError = Spanned<&'static str>;

pub fn decode_dec(input: &str) -> Dec {
    let (whole, fract, exp) = match input.split_once('.') {
        Some((whole, rest)) => {
            let (fract, exp) = decode_exp(rest);
            (whole, Some(fract), exp)
        }
        None => {
            let (whole, exp) = decode_exp(input);
            (whole, None, exp)
        }
    };
    Dec {
        whole: whole.to_owned(),
        fract: fract.map(ToOwned::to_owned),
        exp,
    }
}

fn decode_exp(input: &str) -> (&str, Option<Exp>) {
    match input.split_once('e') {
        Some((init, exp)) => {
            let (is_neg, digits) = take_neg(exp);
            (
                init,
                Some(Exp {
                    is_neg,
                    digits: digits.to_owned(),
                }),
            )
        }
        None => (input, None),
    }
}

pub fn decode_float(input: &str) -> Float {
    // Consume leading `-`.
    let (is_neg, rest) = match input.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, input),
    };

    // Skip `0`.
    let rest = &rest[1..];

    let (value, rest) = match rest.chars().next().expect("unexpected end of float") {
        'x' => {
            let rest = &rest[1..];
            let (value_input, rest) = rest
                .rsplit_once('f')
                .expect("missing float significand size");
            let (value_rest, exp) = value_input
                .rsplit_once('e')
                .expect("missing float exponent");
            let (exp_is_neg, exp_digits) = take_neg(exp);
            let (whole, fract) = value_rest
                .split_once('.')
                .expect("missing float fractional part");
            let value = PlainFloatValue {
                is_neg,
                whole: whole.to_owned(),
                fract: fract.to_owned(),
                exp: Exp {
                    is_neg: exp_is_neg,
                    digits: exp_digits.to_owned(),
                },
            };
            (value.into(), rest)
        }
        // `NaN...`
        'N' | 'n' => (FloatValue::NotANumber, &rest[3..]),
        // `+oo...`
        '+' => (FloatValue::PosInfinity, &rest[3..]),
        // `-oo...`
        '-' => (FloatValue::NegInfinity, &rest[3..]),
        _ => panic!("malformed float"),
    };

    let (sig_size, exp_size) = rest.split_once('e').expect("missing float exponent size");

    Float {
        value,
        sig_size: sig_size.to_owned(),
        exp_size: exp_size.to_owned(),
    }
}

pub fn decode_bv_lit(input: &str) -> BvLit {
    let (n, width) = input.split_once("bv").expect("malformed bitvector literal");
    BvLit {
        n: n.to_owned(),
        width: width.to_owned(),
    }
}

fn take_neg(input: &str) -> (bool, &str) {
    match input.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, input),
    }
}
