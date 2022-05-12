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
type #Int32 = bv32;
type #Int64 = bv64;
type #Double = float53e11; // 64-bit; see https://github.com/boogie-org/boogie/issues/29#issuecomment-231239065

// Documentation on the available built-in functions can be found at:
// https://boogie-docs.readthedocs.io/en/latest/LangRef.html#other-operators

function {:bvbuiltin "(_ sign_extend 32)"} #Int32^to#Int64(n: #Int32): #Int64;
function {:bvbuiltin "(_ extract 31 0)"} #Int32^from#Int64(n: #Int64): #Int32;

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

function {:bvbuiltin "fp.neg"} #Double^negate(n: #Double): #Double;
function {:bvbuiltin "fp.add RNE"} #Double^add(x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.sub RNE"} #Double^sub(x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.mul RNE"} #Double^mul(x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.div RNE"} #Double^div(x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.lt"} #Double^lt(x: #Double, y: #Double): #Bool;
function {:bvbuiltin "fp.leq"} #Double^lte(x: #Double, y: #Double): #Bool;
function {:bvbuiltin "fp.gt"} #Double^gt(x: #Double, y: #Double): #Bool;
function {:bvbuiltin "fp.geq"} #Double^gte(x: #Double, y: #Double): #Bool;

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

// See the heading "Conversion from other sorts":
// https://smtlib.cs.uiowa.edu/theories-FloatingPoint.shtml
function {:builtin "(_ to_fp 11 53) RNE"} #Double~from_i32(n: #Int32): #Double;

const #Double~INFINITY: #Double;
axiom #Double~INFINITY == 0+oo53e11;

const #Double~NEG_INFINITY: #Double;
axiom #Double~NEG_INFINITY == 0-oo53e11;

function {:builtin "fp.isNaN"} #Double~is_nan(n: #Double): #Bool;

// ... end prelude ...
