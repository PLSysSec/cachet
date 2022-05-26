// ... begin prelude ...

type Pc = int;

type {:datatype} EmitPath;
function {:constructor} NilEmitPath(): EmitPath;
function {:constructor} ConsEmitPath(init: EmitPath, last: int): EmitPath;

type #Unit;
const #unit: #Unit;
axiom (forall x: #Unit, y: #Unit :: x == y);

// TODO(spinda): Check the validity of handling `Bool` like a numeric type in
// Boogie.
type #Bool = bool;

type #UInt16 = bv16;
type #Int16 = bv16;
type #Int32 = bv32;
type #UInt32 = bv32;
type #Int64 = bv64;
type #UInt64 = bv64;
type #Double = float53e11; // 64-bit; see https://github.com/boogie-org/boogie/issues/29#issuecomment-231239065

// Documentation on the available built-in functions can be found at:
// https://boogie-docs.readthedocs.io/en/latest/LangRef.html#other-operators

// Conversions to and from double
function {:builtin "(_ fp.to_sbv 64) RNE"}  #Double^to#Int64 (n: #Double): #Int64;
function {:builtin "(_ fp.to_sbv 32) RNE"}  #Double^to#Int32 (n: #Double): #Int32;
function {:builtin "(_ fp.to_sbv 16) RNE"}  #Double^to#Int16 (n: #Double): #Int16;
function {:builtin "(_ fp.to_ubv 64) RNE"}  #Double^to#UInt64 (n: #Double): #UInt64;
function {:builtin "(_ fp.to_ubv 32) RNE"}  #Double^to#UInt32 (n: #Double): #UInt32;
function {:builtin "(_ fp.to_ubv 16) RNE"}  #Double^to#UInt16 (n: #Double): #UInt16;


function {:bvbuiltin "(_ extract 15 0)"}   #Int64^to#Int16 (n: #Int64): #Int16;
function {:bvbuiltin "(_ extract 15 0)"}   #Int64^to#UInt16(n: #Int64): #UInt16;
function {:bvbuiltin "(_ extract 31 0)"}   #Int64^to#Int32 (n: #Int64): #Int32;
function {:bvbuiltin "(_ extract 31 0)"}   #Int64^to#UInt32(n: #Int64): #UInt32;
function                                   #Int64^to#UInt64(n: #Int64): #UInt64 { n }
function {:builtin "(_ to_fp 11 53) RNE"}  #Int64^to#Double(n: #Int64): #Double;

function {:bvbuiltin "(_ extract 15 0)"}   #Int32^to#Int16 (n: #Int32): #Int16;
function {:bvbuiltin "(_ extract 15 0)"}   #Int32^to#UInt16(n: #Int32): #UInt16;
function                                   #Int32^to#UInt32(n: #Int32): #UInt32 { n }
function {:bvbuiltin "(_ sign_extend 32)"} #Int32^to#Int64 (n: #Int32): #Int64;
function {:bvbuiltin "(_ sign_extend 32)"} #Int32^to#UInt64(n: #Int32): #UInt64;
function {:builtin "(_ to_fp 11 53) RNE"}  #Int32^to#Double(n: #Int32): #Double;

function                                   #Int16^to#UInt16(n: #Int16): #UInt16 { n }
function {:bvbuiltin "(_ sign_extend 16)"} #Int16^to#Int32 (n: #Int16): #Int32;
function {:bvbuiltin "(_ sign_extend 16)"} #Int16^to#UInt32(n: #Int16): #UInt32;
function {:bvbuiltin "(_ sign_extend 48)"} #Int16^to#Int64 (n: #Int16): #Int64;
function {:bvbuiltin "(_ sign_extend 48)"} #Int16^to#UInt64(n: #Int16): #UInt64;
function {:builtin "(_ to_fp 11 53) RNE"}  #Int16^to#Double(n: #Int16): #Double;

function                                   #UInt16^to#Int16 (n: #UInt16): #UInt16 { n }
function {:bvbuiltin "(_ zero_extend 16)"} #UInt16^to#Int32 (n: #UInt16): #Int32;
function {:bvbuiltin "(_ zero_extend 16)"} #UInt16^to#UInt32(n: #UInt16): #UInt32;
function {:bvbuiltin "(_ zero_extend 48)"} #UInt16^to#Int64 (n: #UInt16): #Int64;
function {:bvbuiltin "(_ zero_extend 48)"} #UInt16^to#UInt64(n: #UInt16): #UInt64;
function {:builtin "(_ to_fp_unsigned 11 53) RNE"}  #UInt16^to#Double(n: #UInt16): #Double;

function {:bvbuiltin "(_ extract 15 0)"}   #UInt32^to#Int16 (n: #UInt32): #Int16;
function {:bvbuiltin "(_ extract 15 0)"}   #UInt32^to#UInt16(n: #UInt32): #UInt16;
function                                   #UInt32^to#Int32 (n: #UInt32): #Int32 { n }
function {:bvbuiltin "(_ zero_extend 32)"} #UInt32^to#Int64 (n: #UInt32): #Int64;
function {:bvbuiltin "(_ zero_extend 32)"} #UInt32^to#UInt64(n: #UInt32): #UInt64;
function {:builtin "(_ to_fp_unsigned 11 53) RNE"}  #UInt32^to#Double(n: #UInt32): #Double;

function {:bvbuiltin "(_ extract 15 0)"}   #UInt64^to#Int16 (n: #UInt64): #Int16;
function {:bvbuiltin "(_ extract 15 0)"}   #UInt64^to#UInt16(n: #UInt64): #UInt16;
function {:bvbuiltin "(_ extract 31 0)"}   #UInt64^to#Int32 (n: #UInt64): #Int32;
function {:bvbuiltin "(_ extract 31 0)"}   #UInt64^to#UInt32(n: #UInt64): #UInt32;
function                                   #UInt64^to#Int64 (n: #UInt64): #Int64 { n }
function {:builtin "(_ to_fp_unsigned 11 53) RNE"}  #UInt64^to#Double(n: #UInt64): #Double;

function {:bvbuiltin "bvneg"} #Int32^negate(n: #Int32): #Int32;
function {:bvbuiltin "bvadd"} #Int32^add(x: #Int32, y: #Int32): #Int32;
function {:bvbuiltin "bvsub"} #Int32^sub(x: #Int32, y: #Int32): #Int32;
function {:bvbuiltin "bvmul"} #Int32^mul(x: #Int32, y: #Int32): #Int32;
function {:bvbuiltin "bvsdiv"} #Int32^div(x: #Int32, y: #Int32): #Int32;
function {:bvbuiltin "bvsle"} #Int32^lte(a: #Int32, y: #Int32): #Bool;
function {:bvbuiltin "bvsge"} #Int32^gte(a: #Int32, y: #Int32): #Bool;
function {:bvbuiltin "bvslt"} #Int32^lt(a: #Int32, y: #Int32): #Bool;
function {:bvbuiltin "bvsgt"} #Int32^gt(a: #Int32, y: #Int32): #Bool;
function {:bvbuiltin "bvor"} #Int32^bitOr(a: #Int32, y: #Int32): #Int32;
function {:bvbuiltin "bvand"} #Int32^bitAnd(a: #Int32, y: #Int32): #Int32;
function {:bvbuiltin "bvxor"} #Int32^xor(a: #Int32, y: #Int32): #Int32;
function {:bvbuiltin "bvshl"} #Int32^shl(a: #Int32, y: #Int32): #Int32;
function {:bvbuiltin "bvlshr"} #Int32^shr(a: #Int32, y: #Int32): #Int32;

function {:bvbuiltin "fp.neg"} #Double^negate(n: #Double): #Double;
function {:bvbuiltin "fp.add RNE"} #Double^add(x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.sub RNE"} #Double^sub(x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.mul RNE"} #Double^mul(x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.div RNE"} #Double^div(x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.eq"} #Double^eq(x: #Double, y: #Double): #Bool;
function {:bvbuiltin "fp.leq"} #Double^lte(x: #Double, y: #Double): #Bool;
function {:bvbuiltin "fp.geq"} #Double^gte(x: #Double, y: #Double): #Bool;
function {:bvbuiltin "fp.lt"} #Double^lt(x: #Double, y: #Double): #Bool;
function {:bvbuiltin "fp.gt"} #Double^gt(x: #Double, y: #Double): #Bool;

function {:bvbuiltin "bvneg"} #Int64^negate(n: #Int64): #Int64;
function {:bvbuiltin "bvadd"} #Int64^add(x: #Int64, y: #Int64): #Int64;
function {:bvbuiltin "bvsub"} #Int64^sub(x: #Int64, y: #Int64): #Int64;
function {:bvbuiltin "bvmul"} #Int64^mul(x: #Int64, y: #Int64): #Int64;
function {:bvbuiltin "bvsdiv"} #Int64^div(x: #Int64, y: #Int64): #Int64;
function {:bvbuiltin "bvsle"} #Int64^lte(a: #Int64, y: #Int64): #Bool;
function {:bvbuiltin "bvsge"} #Int64^gte(a: #Int64, y: #Int64): #Bool;
function {:bvbuiltin "bvslt"} #Int64^lt(a: #Int64, y: #Int64): #Bool;
function {:bvbuiltin "bvsgt"} #Int64^gt(a: #Int64, y: #Int64): #Bool;
function {:bvbuiltin "bvor"} #Int64^bitOr(a: #Int64, y: #Int64): #Int64;
function {:bvbuiltin "bvand"} #Int64^bitAnd(a: #Int64, y: #Int64): #Int64;
function {:bvbuiltin "bvxor"} #Int64^xor(a: #Int64, y: #Int64): #Int64;
function {:bvbuiltin "bvshl"} #Int64^shl(a: #Int64, y: #Int64): #Int64;
function {:bvbuiltin "bvlshr"} #Int64^shr(a: #Int64, y: #Int64): #Int64;

function {:bvbuiltin "bvneg"} #UInt16^negate(n: #UInt16): #UInt16;
function {:bvbuiltin "bvadd"} #UInt16^add(x: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvsub"} #UInt16^sub(x: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvmul"} #UInt16^mul(x: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvudiv"} #UInt16^div(x: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvsle"} #UInt16^lte(a: #UInt16, y: #UInt16): #Bool;
function {:bvbuiltin "bvsge"} #UInt16^gte(a: #UInt16, y: #UInt16): #Bool;
function {:bvbuiltin "bvslt"} #UInt16^lt(a: #UInt16, y: #UInt16): #Bool;
function {:bvbuiltin "bvsgt"} #UInt16^gt(a: #UInt16, y: #UInt16): #Bool;
function {:bvbuiltin "bvor"} #UInt16^bitOr(a: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvand"} #UInt16^bitAnd(a: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvxor"} #UInt16^xor(a: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvshl"} #UInt16^shl(a: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvlshr"} #UInt16^shr(a: #UInt16, y: #UInt16): #UInt16;

function {:bvbuiltin "bvneg"} #Int16^negate(n: #Int16): #Int16;
function {:bvbuiltin "bvadd"} #Int16^add(x: #Int16, y: #Int16): #Int16;
function {:bvbuiltin "bvsub"} #Int16^sub(x: #Int16, y: #Int16): #Int16;
function {:bvbuiltin "bvmul"} #Int16^mul(x: #Int16, y: #Int16): #Int16;
function {:bvbuiltin "bvudiv"} #Int16^div(x: #Int16, y: #Int16): #Int16;
function {:bvbuiltin "bvsle"} #Int16^lte(a: #Int16, y: #Int16): #Bool;
function {:bvbuiltin "bvsge"} #Int16^gte(a: #Int16, y: #Int16): #Bool;
function {:bvbuiltin "bvslt"} #Int16^lt(a: #Int16, y: #Int16): #Bool;
function {:bvbuiltin "bvsgt"} #Int16^gt(a: #Int16, y: #Int16): #Bool;
function {:bvbuiltin "bvor"} #Int16^bitOr(a: #Int16, y: #Int16): #Int16;
function {:bvbuiltin "bvand"} #Int16^bitAnd(a: #Int16, y: #Int16): #Int16;
function {:bvbuiltin "bvxor"} #Int16^xor(a: #Int16, y: #Int16): #Int16;
function {:bvbuiltin "bvshl"} #Int16^shl(a: #Int16, y: #Int16): #Int16;
function {:bvbuiltin "bvlshr"} #Int16^shr(a: #Int16, y: #Int16): #Int16;

function {:bvbuiltin "bvneg"} #UInt32^negate(n: #UInt32): #UInt32;
function {:bvbuiltin "bvadd"} #UInt32^add(x: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvsub"} #UInt32^sub(x: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvmul"} #UInt32^mul(x: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvudiv"} #UInt32^div(x: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvsle"} #UInt32^lte(a: #UInt32, y: #UInt32): #Bool;
function {:bvbuiltin "bvsge"} #UInt32^gte(a: #UInt32, y: #UInt32): #Bool;
function {:bvbuiltin "bvslt"} #UInt32^lt(a: #UInt32, y: #UInt32): #Bool;
function {:bvbuiltin "bvsgt"} #UInt32^gt(a: #UInt32, y: #UInt32): #Bool;
function {:bvbuiltin "bvor"} #UInt32^bitOr(a: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvand"} #UInt32^bitAnd(a: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvxor"} #UInt32^xor(a: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvshl"} #UInt32^shl(a: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvlshr"} #UInt32^shr(a: #UInt32, y: #UInt32): #UInt32;

function {:bvbuiltin "bvneg"} #UInt64^negate(n: #UInt64): #UInt64;
function {:bvbuiltin "bvadd"} #UInt64^add(x: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvsub"} #UInt64^sub(x: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvmul"} #UInt64^mul(x: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvudiv"} #UInt64^div(x: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvsle"} #UInt64^lte(a: #UInt64, y: #UInt64): #Bool;
function {:bvbuiltin "bvsge"} #UInt64^gte(a: #UInt64, y: #UInt64): #Bool;
function {:bvbuiltin "bvslt"} #UInt64^lt(a: #UInt64, y: #UInt64): #Bool;
function {:bvbuiltin "bvsgt"} #UInt64^gt(a: #UInt64, y: #UInt64): #Bool;
function {:bvbuiltin "bvor"} #UInt64^bitOr(a: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvand"} #UInt64^bitAnd(a: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvxor"} #UInt64^xor(a: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvshl"} #UInt64^shl(a: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvlshr"} #UInt64^shr(a: #UInt64, y: #UInt64): #UInt64;

type #Map k v = [k]v;

function {:inline} #Map~get<k, v>(map: #Map k v, key: k): v {
  map[key]
}

function {:inline} #Map~set<k, v>(map: #Map k v, key: k, value: v): #Map k v {
  map[key := value]
}

type #Set a = #Map a #Bool;

function {:inline} #Set~contains<a>(set: #Set a, value: a): #Bool {
  #Map~get(set, value)
}

function {:inline} #Set~add<a>(set: #Set a, value: a): #Set a {
  #Map~set(set, value, true)
}

function {:inline} #Set~remove<a>(set: #Set a, value: a): #Set a {
  #Map~set(set, value, false)
}

// Impls for cachet's prelude...


const #Double~INFINITY: #Double;
axiom #Double~INFINITY == 0+oo53e11;

const #Double~NEG_INFINITY: #Double;
axiom #Double~NEG_INFINITY == 0-oo53e11;


function {:builtin "fp.isNaN"} #Double~is_nan(n: #Double): #Bool;
function {:bvbuiltin "fp.roundToIntegral RTP"} #Double~ceil(x: #Double): #Double;

// "There is no function for converting from (_ FloatingPoint eb sb) to the
//  corresponding IEEE 754-2008 binary format, as a bit vector (_ BitVec m) with 
//  m = eb + sb, because (_ NaN eb sb) has multiple, well-defined representations.
//  Instead, an encoding of the kind below is recommended, where f is a term
//  of sort (_ FloatingPoint eb sb):
//
// (declare-fun b () (_ BitVec m))
// (assert (= ((_ to_fp eb sb) b) f))
//"
//
// Copied from https://smtlib.cs.uiowa.edu/theories-FloatingPoint.shtml
function #Double~bits(n: #Double): #UInt64;
function {:builtin "(_ to_fp 11 53)"} ReinterpretUInt64AsDouble(d: #UInt64): #Double;
axiom (forall d: #Double :: ReinterpretUInt64AsDouble(#Double~bits(d)) == d);

// ... end prelude ...


type #Range;

function #Range^field~lower_(instance: #Range): #Int32;

function #Range^field~upper_(instance: #Range): #Int32;

function #Range^field~hasInt32LowerBound_(instance: #Range): #Bool;

function #Range^field~hasInt32UpperBound_(instance: #Range): #Bool;

function #Range^field~canHaveFractionalPart_(instance: #Range): #Bool;

function #Range^field~canBeNegativeZero_(instance: #Range): #Bool;

function #Range^field~max_exponent_(instance: #Range): #UInt16;

type #JSValue;

function #UInt16~max(): #UInt16{  65535bv16}

function #Range~maxInt32Exponent(): #UInt16{  31bv16}

function #Range~maxUInt32Exponent(): #UInt16{  31bv16}

function #Range~maxTruncatableExponent(): #UInt16{  53bv16}

function #Range~maxFiniteExponent(): #UInt16{  1023bv16}

function #Range~includesInfinity(): #UInt16{  #UInt16^add(#Range~maxFiniteExponent(), 1bv16)}

function #Range~includesInfinityAndNaN(): #UInt16{  #UInt16~max()}

function #Range~noInt32UpperBound(): #Int64{  #Int64^add(#Int32^to#Int64(#JSValue~intMax()), 1bv64)}

function #Range~noInt32LowerBound(): #Int64{  #Int64^sub(#Int32^to#Int64(#JSValue~intMin()), 1bv64)}

function #doubleExpBias(): #Int16{  #UInt16^to#Int16(#UInt16^sub(#UInt16^shl(1bv16, 10bv16), 1bv16))}

function #JSValue~intMax(): #Int32{  #UInt32^to#Int32(2147483647bv32)}

function #JSValue~intMin(): #Int32{  #UInt32^to#Int32(2147483648bv32)}

procedure #Int16~abs($n: #Int16)
  returns (ret: #Int16)
{
  if (#Int16^lt($n, 0bv16)) {
    ret := #Int16^negate($n);
    return;
  } else {
    ret := $n;
    return;
  }
}

procedure #Int32~abs($n: #Int32)
  returns (ret: #Int32)
{
  if (#Int32^lt($n, 0bv32)) {
    ret := #Int32^negate($n);
    return;
  } else {
    ret := $n;
    return;
  }
}

procedure #Int32~max($a: #Int32, $b: #Int32)
  returns (ret: #Int32)
{
  if (#Int32^lt($a, $b)) {
    ret := $b;
    return;
  } else {
    ret := $a;
    return;
  }
}

procedure #Range~exponentImpliedByInt32Bounds($r: #Range)
  returns (ret: #UInt16)
{
  var $max'v0: #Int32;
  var $tmp'v1: #Int32;
  var $tmp'v2: #Int32;
  var out'0: #Int32;
  var out'1: #Int32;
  var out'2: #Int32;
  var out'3: #UInt32;
  
  call out'0 := #Int32~abs(#Range^field~lower_($r));
  $tmp'v1 := out'0;
  call out'1 := #Int32~abs(#Range^field~upper_($r));
  $tmp'v2 := out'1;
  call out'2 := #Int32~max($tmp'v1, $tmp'v2);
  $max'v0 := out'2;
  call out'3 := #floor_log_2(#Int32^to#UInt32($max'v0));
  ret := #UInt32^to#UInt16(out'3);
  return;
}

procedure #Range~hasInt32Bounds($r: #Range)
  returns (ret: #Bool)
{
  var $tmp'v0: #Bool;
  
  $tmp'v0 := false;
  if (#Range^field~hasInt32LowerBound_($r)) {
    $tmp'v0 := #Range^field~hasInt32UpperBound_($r);
  }
  ret := $tmp'v0;
  return;
}

procedure #Range~ceil($r: #Range)
  returns (ret: #Range)
{
  var $max_exponent_'v0: #UInt16;
  var $canBeNegativeZero_'v1: #Bool;
  var $canHaveFractionalPart_'v2: #Bool;
  var $tmp'v3: #UInt16;
  var $tmp'v4: #UInt16;
  var $tmp'v5: #Bool;
  var $tmp'v6: #Bool;
  var $tmp'v7: #UInt16;
  var out'0: #Bool;
  var out'1: #UInt16;
  
  $max_exponent_'v0 := #Range^field~max_exponent_($r);
  $tmp'v3 := $max_exponent_'v0;
  call out'0 := #Range~hasInt32Bounds($r);
  if (out'0) {
    call out'1 := #Range~exponentImpliedByInt32Bounds($r);
    $max_exponent_'v0 := out'1;
  } else if (#UInt16^lt($tmp'v3, #Range~maxFiniteExponent())) {
    $tmp'v4 := $max_exponent_'v0;
    $max_exponent_'v0 := #UInt16^add($tmp'v4, 1bv16);
  }
  $tmp'v6 := true;
  if (!#Range^field~canBeNegativeZero_($r)) {
    $tmp'v5 := false;
    if (#Int32^lte(#Range^field~lower_($r), 0bv32)) {
      $tmp'v5 := #Int32^gt(#Range^field~upper_($r), 4294967295bv32);
    }
    $tmp'v6 := $tmp'v5;
  }
  $canBeNegativeZero_'v1 := $tmp'v6;
  $canHaveFractionalPart_'v2 := false;
  $tmp'v7 := $max_exponent_'v0;
  call ret := #mk_range(#Range^field~lower_($r), #Range^field~upper_($r), #Range^field~hasInt32LowerBound_($r), #Range^field~hasInt32UpperBound_($r), $canHaveFractionalPart_'v2, $canBeNegativeZero_'v1, $tmp'v7);
  return;
}

procedure #Range~in_range($r: #Range, $n: #Double)
  returns (ret: #Bool)
{
  var $exp'v0: #Int16;
  var $tmp'v1: #Bool;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var $tmp'v6: #Bool;
  var $tmp'v7: #Bool;
  var $tmp'v8: #UInt16;
  var out'0: #Bool;
  var out'1: #Int16;
  var out'2: #Int16;
  
  $tmp'v2 := false;
  call out'0 := #is_inf($n);
  $tmp'v1 := out'0;
  if ($tmp'v1) {
    $tmp'v2 := #UInt16^lt(#Range^field~max_exponent_($r), #Range~includesInfinity());
  }
  if ($tmp'v2) {
    ret := false;
    return;
  }
  $tmp'v4 := false;
  $tmp'v3 := #Double~is_nan($n);
  if ($tmp'v3) {
    $tmp'v4 := (#Range^field~max_exponent_($r) != #Range~includesInfinityAndNaN());
  }
  if ($tmp'v4) {
    ret := false;
    return;
  }
  $tmp'v5 := false;
  if (!#Range^field~canBeNegativeZero_($r)) {
    $tmp'v5 := ($n == -0x0.0e-269f53e11);
  }
  if ($tmp'v5) {
    ret := false;
    return;
  }
  $tmp'v6 := false;
  if (#Range^field~hasInt32LowerBound_($r)) {
    $tmp'v6 := #Double^lt($n, #Int32^to#Double(#Range^field~lower_($r)));
  }
  if ($tmp'v6) {
    ret := false;
    return;
  }
  $tmp'v7 := false;
  if (#Range^field~hasInt32UpperBound_($r)) {
    $tmp'v7 := #Double^lt(#Int32^to#Double(#Range^field~upper_($r)), $n);
  }
  if ($tmp'v7) {
    ret := false;
    return;
  }
  call out'1 := #get_exp($n);
  $exp'v0 := out'1;
  call out'2 := #Int16~abs($exp'v0);
  $tmp'v8 := #Int16^to#UInt16(out'2);
  if (#UInt16^gt($tmp'v8, #Range^field~max_exponent_($r))) {
    ret := false;
    return;
  }
  ret := true;
  return;
}

procedure #Range~well_formed($r: #Range)
  returns (ret: #Bool)
{
  var $tmp'v0: #Bool;
  var $tmp'v1: #Bool;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var $tmp'v6: #Bool;
  var $tmp'v7: #Bool;
  var $tmp'v8: #Bool;
  var $tmp'v9: #UInt16;
  var $tmp'v10: #Bool;
  var $tmp'v11: #Bool;
  var $tmp'v12: #UInt32;
  var $tmp'v13: #UInt32;
  var $tmp'v14: #Bool;
  var $tmp'v15: #Bool;
  var $tmp'v16: #UInt32;
  var $tmp'v17: #UInt32;
  var $tmp'v18: #Bool;
  var $tmp'v19: #Bool;
  var out'0: #Bool;
  var out'1: #UInt16;
  var out'2: #Int32;
  var out'3: #UInt32;
  var out'4: #Int32;
  var out'5: #UInt32;
  
  $tmp'v0 := true;
  if (!#Int32^lt(#Range^field~lower_($r), #JSValue~intMin())) {
    $tmp'v0 := #Int32^gt(#Range^field~lower_($r), #JSValue~intMax());
  }
  if ($tmp'v0) {
    ret := false;
    return;
  }
  $tmp'v1 := true;
  if (!#Int32^lt(#Range^field~upper_($r), #JSValue~intMin())) {
    $tmp'v1 := #Int32^gt(#Range^field~upper_($r), #JSValue~intMax());
  }
  if ($tmp'v1) {
    ret := false;
    return;
  }
  if (#Int32^gt(#Range^field~lower_($r), #Range^field~upper_($r))) {
    ret := false;
    return;
  }
  $tmp'v2 := false;
  if (!#Range^field~hasInt32LowerBound_($r)) {
    $tmp'v2 := (#Range^field~lower_($r) != #JSValue~intMin());
  }
  if ($tmp'v2) {
    ret := false;
    return;
  }
  $tmp'v3 := false;
  if (!#Range^field~hasInt32UpperBound_($r)) {
    $tmp'v3 := (#Range^field~upper_($r) != #JSValue~intMax());
  }
  if ($tmp'v3) {
    ret := false;
    return;
  }
  $tmp'v5 := true;
  $tmp'v4 := false;
  if (#Range^field~canBeNegativeZero_($r)) {
    $tmp'v4 := #Int32^gt(#Range^field~lower_($r), 0bv32);
  }
  if (!$tmp'v4) {
    $tmp'v5 := #Int32^lt(#Range^field~upper_($r), 0bv32);
  }
  if ($tmp'v5) {
    ret := false;
    return;
  }
  $tmp'v7 := true;
  $tmp'v6 := true;
  if (!#UInt16^lte(#Range^field~max_exponent_($r), #Range~maxFiniteExponent())) {
    $tmp'v6 := (#Range^field~max_exponent_($r) == #Range~includesInfinity());
  }
  if (!$tmp'v6) {
    $tmp'v7 := (#Range^field~max_exponent_($r) == #Range~includesInfinityAndNaN());
  }
  if (!$tmp'v7) {
    ret := false;
    return;
  }
  $tmp'v11 := false;
  call out'0 := #Range~hasInt32Bounds($r);
  $tmp'v8 := out'0;
  if ($tmp'v8) {
    call out'1 := #Range~exponentImpliedByInt32Bounds($r);
    $tmp'v9 := out'1;
    $tmp'v10 := (#Range^field~max_exponent_($r) != $tmp'v9);
    $tmp'v11 := $tmp'v10;
  }
  if ($tmp'v11) {
    ret := false;
    return;
  }
  $tmp'v15 := false;
  if (#Range^field~hasInt32LowerBound_($r)) {
    call out'2 := #Int32~abs(#Range^field~lower_($r));
    $tmp'v12 := #Int32^to#UInt32(out'2);
    call out'3 := #floor_log_2($tmp'v12);
    $tmp'v13 := out'3;
    $tmp'v14 := #UInt32^lt(#UInt16^to#UInt32(#Range^field~max_exponent_($r)), $tmp'v13);
    $tmp'v15 := $tmp'v14;
  }
  if ($tmp'v15) {
    ret := false;
    return;
  }
  $tmp'v19 := false;
  if (#Range^field~hasInt32UpperBound_($r)) {
    call out'4 := #Int32~abs(#Range^field~upper_($r));
    $tmp'v16 := #Int32^to#UInt32(out'4);
    call out'5 := #floor_log_2($tmp'v16);
    $tmp'v17 := out'5;
    $tmp'v18 := #UInt32^lt(#UInt16^to#UInt32(#Range^field~max_exponent_($r)), $tmp'v17);
    $tmp'v19 := $tmp'v18;
  }
  if ($tmp'v19) {
    ret := false;
    return;
  }
  ret := true;
  return;
}

function #mk_range_raw($lower_: #Int32, $upper_: #Int32, $hasInt32LowerBound_: #Bool, $hasInt32UpperBound_: #Bool, $canHaveFractionalPart_: #Bool, $canBeNegativeZero_: #Bool, $max_exponent_: #UInt16): #Range;

procedure #mk_range($lower_: #Int32, $upper_: #Int32, $hasInt32LowerBound_: #Bool, $hasInt32UpperBound_: #Bool, $canHaveFractionalPart_: #Bool, $canBeNegativeZero_: #Bool, $max_exponent_: #UInt16)
  returns (ret: #Range)
{
  var $r'v0: #Range;
  
  $r'v0 := #mk_range_raw($lower_, $upper_, $hasInt32LowerBound_, $hasInt32UpperBound_, $canHaveFractionalPart_, $canBeNegativeZero_, $max_exponent_);
  assume (#Range^field~lower_($r'v0) == $lower_);
  assume (#Range^field~upper_($r'v0) == $upper_);
  assume (#Range^field~hasInt32LowerBound_($r'v0) == $hasInt32LowerBound_);
  assume (#Range^field~hasInt32UpperBound_($r'v0) == $hasInt32UpperBound_);
  assume (#Range^field~canHaveFractionalPart_($r'v0) == $canHaveFractionalPart_);
  assume (#Range^field~canBeNegativeZero_($r'v0) == $canBeNegativeZero_);
  assume (#Range^field~max_exponent_($r'v0) == $max_exponent_);
  ret := $r'v0;
  return;
}

procedure #floor_log_2($n: #UInt32)
  returns (ret: #UInt32)
{
  if (#UInt32^lt($n, 2bv32)) {
    ret := 0bv32;
    return;
  } else if (#UInt32^lt($n, 4bv32)) {
    ret := 1bv32;
    return;
  } else if (#UInt32^lt($n, 8bv32)) {
    ret := 2bv32;
    return;
  } else if (#UInt32^lt($n, 16bv32)) {
    ret := 3bv32;
    return;
  } else if (#UInt32^lt($n, 32bv32)) {
    ret := 4bv32;
    return;
  } else if (#UInt32^lt($n, 64bv32)) {
    ret := 5bv32;
    return;
  } else if (#UInt32^lt($n, 128bv32)) {
    ret := 6bv32;
    return;
  } else if (#UInt32^lt($n, 256bv32)) {
    ret := 7bv32;
    return;
  } else if (#UInt32^lt($n, 512bv32)) {
    ret := 8bv32;
    return;
  } else if (#UInt32^lt($n, 1024bv32)) {
    ret := 9bv32;
    return;
  } else if (#UInt32^lt($n, 2048bv32)) {
    ret := 10bv32;
    return;
  } else if (#UInt32^lt($n, 4096bv32)) {
    ret := 11bv32;
    return;
  } else if (#UInt32^lt($n, 8192bv32)) {
    ret := 12bv32;
    return;
  } else if (#UInt32^lt($n, 16384bv32)) {
    ret := 13bv32;
    return;
  } else if (#UInt32^lt($n, 32768bv32)) {
    ret := 14bv32;
    return;
  } else if (#UInt32^lt($n, 65536bv32)) {
    ret := 15bv32;
    return;
  } else if (#UInt32^lt($n, 131072bv32)) {
    ret := 16bv32;
    return;
  } else if (#UInt32^lt($n, 262144bv32)) {
    ret := 17bv32;
    return;
  } else if (#UInt32^lt($n, 524288bv32)) {
    ret := 18bv32;
    return;
  } else if (#UInt32^lt($n, 1048576bv32)) {
    ret := 19bv32;
    return;
  } else if (#UInt32^lt($n, 2097152bv32)) {
    ret := 20bv32;
    return;
  } else if (#UInt32^lt($n, 4194304bv32)) {
    ret := 21bv32;
    return;
  } else if (#UInt32^lt($n, 8388608bv32)) {
    ret := 22bv32;
    return;
  } else if (#UInt32^lt($n, 16777216bv32)) {
    ret := 23bv32;
    return;
  } else if (#UInt32^lt($n, 33554432bv32)) {
    ret := 24bv32;
    return;
  } else if (#UInt32^lt($n, 67108864bv32)) {
    ret := 25bv32;
    return;
  } else if (#UInt32^lt($n, 134217728bv32)) {
    ret := 26bv32;
    return;
  } else if (#UInt32^lt($n, 268435456bv32)) {
    ret := 27bv32;
    return;
  } else if (#UInt32^lt($n, 536870912bv32)) {
    ret := 28bv32;
    return;
  } else if (#UInt32^lt($n, 1073741824bv32)) {
    ret := 29bv32;
    return;
  } else if (#UInt32^lt($n, 2147483648bv32)) {
    ret := 30bv32;
    return;
  } else {
    ret := 31bv32;
    return;
  }
}

procedure #get_exp($n: #Double)
  returns (ret: #Int16)
{
  var $bits'v0: #UInt64;
  var $exp_bits'v1: #Int16;
  var $exp'v2: #Int16;
  var $tmp'v3: #Bool;
  
  $tmp'v3 := true;
  if (!($n == 0x0.0e-269f53e11)) {
    $tmp'v3 := ($n == -0x0.0e-269f53e11);
  }
  if ($tmp'v3) {
    ret := 0bv16;
    return;
  }
  $bits'v0 := #Double~bits($n);
  $exp_bits'v1 := #UInt64^to#Int16(#UInt64^bitAnd(#UInt64^shr($bits'v0, 52bv64), #UInt16^to#UInt64(2047bv16)));
  $exp'v2 := #Int16^sub($exp_bits'v1, #doubleExpBias());
  ret := $exp'v2;
  return;
}

procedure #is_inf($n: #Double)
  returns (ret: #Bool)
{
  var $tmp'v0: #Bool;
  
  $tmp'v0 := true;
  if (!($n == #Double~INFINITY)) {
    $tmp'v0 := ($n == #Double~NEG_INFINITY);
  }
  ret := $tmp'v0;
  return;
}

procedure #ceil_spec($r: #Range, $x: #Double)
{
  var $r2'v0: #Range;
  var $x2'v1: #Double;
  var out'0: #Bool;
  var out'1: #Bool;
  var out'2: #Range;
  var out'3: #Double;
  var out'4: #Bool;
  var out'5: #Bool;
  
  call out'0 := #Range~well_formed($r);
  assume out'0;
  call out'1 := #Range~in_range($r, $x);
  assume out'1;
  call out'2 := #Range~ceil($r);
  $r2'v0 := out'2;
  call out'3 := #JSValue~ceil($x);
  $x2'v1 := out'3;
  call out'4 := #Range~well_formed($r2'v0);
  assert out'4;
  call out'5 := #Range~in_range($r2'v0, $x2'v1);
  assert out'5;
  return;
}

procedure #JSValue~ceil($d: #Double)
  returns (ret: #Double)
{
  ret := #Double~ceil($d);
  return;
}