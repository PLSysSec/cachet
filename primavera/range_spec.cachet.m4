include(`macros.m4')

var r: Range;
var y: Int32;
var x: Double = y;

foreach(`OP', unops, `
fn OP()_spec() {
    assume Range::well_formed(r);
    assume Range::in_range(r, x);

    let r2 = Range::OP()(r);
    let x2 = JSValue::OP()(x);

    assert Range::well_formed(r2);
    assert Range::in_range(r2, x2);
}'
)
