include(`macros.m4')

foreach(`OP', unops, `
fn OP()_spec(r: Range, x: Double) {
    assume Range::well_formed(r);
    assume Range::in_range(r, x);

    let r2 = Range::OP()(r);
    let x2 = JSValue::OP()(x);

    assert Range::well_formed(r2);
    assert Range::in_range(r2, x2);
}'
)
