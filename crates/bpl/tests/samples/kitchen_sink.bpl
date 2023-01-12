axiom {:hello} {:world} {:hello world} {:world yes, "hello", "w\"o\"rld"} {foo} {foo, bar} false;

const foo, bar: bool;

function foo(x: int): [bool]int;

function bar(x: int, y: quux) returns (int);

function baz(x: int, y: bool, z: foo int quux) returns (quux: int);

function quux(): foo;

implementation foo<T>(x: int, y: foo bar baz quux int bool) returns () {
  var z: int;
  var w: real;
  hello:
}

implementation bar<T>(x: int, y: bool) returns (z: real) {}

implementation baz<T>(x: int, y: bool) returns (z: real) {

  hello:

  world:

}

procedure foo<T>(x: int, y: bool) returns ()
modifies bar;
requires false;
ensures false;
{
  var z: real;
  hello:
  world:
  x := y;
  x, y := z;
  x := y, z;
  x[y][z] := x, y, z;
  x[y, z][x, y, z] := x;
  x[x][y, z][][x, y, z] := x, y, z;
  break;
  break hello;
  call foo(x, y, z);
  call bar();
  call x, y := baz(z);
  call x := quux(y, z);
  assume {:aaa} {:bbb} false;
  assert false;
  goto world;
  goto hello, world;
  goto hello, world, hello;
  havoc x;
  havoc x, y;
  havoc x, y, z;
  if (*) {}
  if (false) {
    call foo();
  } else {
    assume false;
  }
  if (x) {
    assert x;
  } else if (y) {
    assert y;
  } else if (z) {
    assert z;
  } else {
    call bar();
  }
  par x := foo();
  par x := foo() | y := bar();
  par x := foo() | y := bar() | z := baz();
  par x := foo() | y := bar() | z := baz() | quux();
  while (*) {}
  while (x)
  invariant y;
  free invariant z;
  {
    call foo();
    call y := bar();
  }
  return;
  yield;
  x := y <==> -x ==> y : int : foo : 27 : foo <bar>[baz]quux ==> z[x][x := y][x, y := z][:= x][][x, y] <== w && (int(x) * 1 + real(y) / 3bv12 div z mod 12 - 6.7 + (7e12 - 3e-10 + 9.87e3 <==> 6) || y || z || foo(bar(baz(x), quux(y), bar()))) && old (if x then y else z) <== (x ==> y ==> z <== w) <== x == false || true <==> !w <==> (forall <x> :: y) <==> (exists x: int :: y) <==> (lambda <x> y: int :: z);
  y := |{
    foo:
      call foo();
      goto bar, baz;
    bar:
      x := 1;
      y := 2;
      goto foo, baz;
    baz:
      return z;
  }|;
  z := |{
    var a, b: int;
    var c: bool;
    foo: goto bar;
    bar: return z;
    baz: goto bar;
  }|;
  w := |{foo:return w;}|;
  x := if x then y else z <==> w;
  y := if x then if y then z else w else q;
  z := if a1 then if b1 then if c1 then c2 else c3 else b3 else if d1 then d2 else if e1 then e2 else e3;
  assert (forall x: bv64, y: bv64, y: bv64 :: true && true && true ==> (foo(x, y) == y || foo(foo(x, y), 1bv64) == y) && (if y == foo(x, y) then bar(x, baz(18446744073709551615bv64, y)) else quux(x, baz(18446744073709551615bv64, y))) ==> foo(bar(baz(x, y), foo(bar(x, y), y)), 0bv32 ++ 63bv32) == 1bv64);
  x := 1 + if x then y else z;
  y := if x then y else z + 1;
  x := 0x1234567890ABCDEFabcdef.1234567890ABCDEFabcdefe-16f32e64;
  x := 0x1234567890ABCDEFabcdef.1234567890ABCDEFabcdefe16f32e64;
  x := 0NaN32e64;
  x := 0nan32e64;
  x := 0+oo32e64;
  x := 0-oo32e64;
}

procedure bar(x: int, y: bool) returns (z: real);

type foo x y = int, bar, baz = bool;
type quux = real;

var foo: int;
