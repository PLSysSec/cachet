//const {:foo foo} foo: int extends foo;

//function {:foo x} foo<T>({:foo foo(foo, 0) : T} foo: T, {:foo foo(foo, 0) : T} bar: int) returns ({:foo foo(x, 0) : T} x: T) { foo }

/*
implementation {:foo quux : T} {:foo baz} {:foo plugh} foo<T>(foo: int, quux: T) returns (bar: int) {
  var baz: int where plugh == 1;
  var {:foo baz} plugh: int where baz == 1;
  bar := baz;
}

procedure {:foo bar, |{ var y: int; hello: call y := foo(bar, quux); return y > 0; }| } foo<T>({:foo foo} foo: int where quux == 1, {:foo foo, |{ var y: int; hello: call y := foo(foo, quux); return y > 0; }|} quux: T where foo == 1) returns (bar: int);
*/
/*
  requires {:foo quux} foo == 1;
  ensures bar == 1;
{
  var baz: int where baz == 1;
  bar := baz;
}
*/

/*
type {:foo foo : foo} {:bar bar : bar} foo, bar, baz = quux, quux;
var foo: foo;
var bar: bar;
*/

/*
var {:foo x} {:foo y} x: int where y == 1, y: int where x == 1;
*/

type {:datatype} EmitPath T;
function {:constructor} NilEmitPath(): EmitPath int;
function {:constructor} ConsEmitPath(init: EmitPath int, last: int): EmitPath int;
