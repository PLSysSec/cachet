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

type #Int8   = bv8;
type #UInt8  = bv8;
type #Int16  = bv16;
type #UInt16 = bv16;
type #Int32  = bv32;
type #UInt32 = bv32;
type #Int64  = bv64;
type #UInt64 = bv64;
type #Double = float53e11; // 64-bit; see https://github.com/boogie-org/boogie/issues/29#issuecomment-231239065

// Documentation on the available built-in functions can be found at:
// https://boogie-docs.readthedocs.io/en/latest/LangRef.html#other-operators
 
function {:bvbuiltin "(_ sign_extend 8   )"}         #Int8^to#Int16   (n: #Int8):   #Int16;
function {:bvbuiltin "(_ sign_extend 24  )"}         #Int8^to#Int32   (n: #Int8):   #Int32;
function {:bvbuiltin "(_ sign_extend 56  )"}         #Int8^to#Int64   (n: #Int8):   #Int64;
function                                             #Int8^to#UInt8   (n: #Int8):   #UInt8 { n }
function {:bvbuiltin "(_ sign_extend 8   )"}         #Int8^to#UInt16  (n: #Int8):   #UInt16;
function {:bvbuiltin "(_ sign_extend 24  )"}         #Int8^to#UInt32  (n: #Int8):   #UInt32;
function {:bvbuiltin "(_ sign_extend 56  )"}         #Int8^to#UInt64  (n: #Int8):   #UInt64;
function {:builtin   "(_ to_fp 11 53) RNE"}          #Int8^to#Double  (n: #Int8):   #Double;

function {:bvbuiltin "(_ extract     7 0 )"}         #Int16^to#Int8   (n: #Int16):  #Int8;
function {:bvbuiltin "(_ sign_extend 16  )"}         #Int16^to#Int32  (n: #Int16):  #Int32;
function {:bvbuiltin "(_ sign_extend 48  )"}         #Int16^to#Int64  (n: #Int16):  #Int64;
function {:bvbuiltin "(_ extract     7 0 )"}         #Int16^to#UInt8  (n: #Int16):  #UInt8;
function                                             #Int16^to#UInt16 (n: #Int16):  #UInt16 { n }
function {:bvbuiltin "(_ sign_extend 16  )"}         #Int16^to#UInt32 (n: #Int16):  #UInt32;
function {:bvbuiltin "(_ sign_extend 48  )"}         #Int16^to#UInt64 (n: #Int16):  #UInt64;
function {:builtin   "(_ to_fp 11 53) RNE"}          #Int16^to#Double (n: #Int16):  #Double;

function {:bvbuiltin "(_ extract     7  0)"}         #Int32^to#Int8   (n: #Int32):  #Int8;
function {:bvbuiltin "(_ extract     15 0)"}         #Int32^to#Int16  (n: #Int32):  #Int16;
function {:bvbuiltin "(_ sign_extend 32  )"}         #Int32^to#Int64  (n: #Int32):  #Int64;
function {:bvbuiltin "(_ extract     7  0)"}         #Int32^to#UInt8  (n: #Int32):  #UInt8;
function {:bvbuiltin "(_ extract     15 0)"}         #Int32^to#UInt16 (n: #Int32):  #UInt16;
function                                             #Int32^to#UInt32 (n: #Int32):  #UInt32 { n }
function {:bvbuiltin "(_ sign_extend 32  )"}         #Int32^to#UInt64 (n: #Int32):  #UInt64;
function {:builtin   "(_ to_fp 11 53) RNE"}          #Int32^to#Double (n: #Int32):  #Double;

function {:bvbuiltin "(_ extract     7  0)"}         #Int64^to#Int8   (n: #Int64):  #Int8;
function {:bvbuiltin "(_ extract     15 0)"}         #Int64^to#Int16  (n: #Int64):  #Int16;
function {:bvbuiltin "(_ extract     31 0)"}         #Int64^to#Int32  (n: #Int64):  #Int32;
function {:bvbuiltin "(_ extract     7  0)"}         #Int64^to#UInt8  (n: #Int64):  #UInt8;
function {:bvbuiltin "(_ extract     15 0)"}         #Int64^to#UInt16 (n: #Int64):  #UInt16;
function {:bvbuiltin "(_ extract     31 0)"}         #Int64^to#UInt32 (n: #Int64):  #UInt32;
function                                             #Int64^to#UInt64 (n: #Int64):  #UInt64 { n }
function {:builtin   "(_ to_fp 11 53) RNE"}          #Int64^to#Double (n: #Int64):  #Double;

function                                             #UInt8^to#Int8   (n: #UInt8):  #Int8 { n }
function {:bvbuiltin "(_ zero_extend 8   )"}         #UInt8^to#Int16  (n: #UInt8):  #Int16;
function {:bvbuiltin "(_ zero_extend 24  )"}         #UInt8^to#Int32  (n: #UInt8):  #Int32;
function {:bvbuiltin "(_ zero_extend 56  )"}         #UInt8^to#Int64  (n: #UInt8):  #Int64;
function {:bvbuiltin "(_ zero_extend 8   )"}         #UInt8^to#UInt16 (n: #UInt8):  #UInt16;
function {:bvbuiltin "(_ zero_extend 24  )"}         #UInt8^to#UInt32 (n: #UInt8):  #UInt32;
function {:bvbuiltin "(_ zero_extend 56  )"}         #UInt8^to#UInt64 (n: #UInt8):  #UInt64;
function {:builtin   "(_ to_fp 11 53) RNE"}          #UInt8^to#Double (n: #UInt8):  #Double;

function {:bvbuiltin "(_ extract     7  0)"}         #UInt16^to#Int8  (n: #UInt16): #Int8;
function                                             #UInt16^to#Int16 (n: #UInt16): #UInt16 { n }
function {:bvbuiltin "(_ zero_extend 16  )"}         #UInt16^to#Int32 (n: #UInt16): #Int32;
function {:bvbuiltin "(_ zero_extend 48  )"}         #UInt16^to#Int64 (n: #UInt16): #Int64;
function {:bvbuiltin "(_ extract     7  0)"}         #UInt16^to#UInt8 (n: #UInt16): #UInt8;
function {:bvbuiltin "(_ zero_extend 16  )"}         #UInt16^to#UInt32(n: #UInt16): #UInt32;
function {:bvbuiltin "(_ zero_extend 48  )"}         #UInt16^to#UInt64(n: #UInt16): #UInt64;
function {:builtin   "(_ to_fp_unsigned 11 53) RNE"} #UInt16^to#Double(n: #UInt16): #Double;

function {:bvbuiltin "(_ extract     7  0)"}         #UInt32^to#Int8  (n: #UInt32): #Int8;
function {:bvbuiltin "(_ extract     15 0)"}         #UInt32^to#Int16 (n: #UInt32): #Int16;
function                                             #UInt32^to#Int32 (n: #UInt32): #Int32 { n }
function {:bvbuiltin "(_ zero_extend 32  )"}         #UInt32^to#Int64 (n: #UInt32): #Int64;
function {:bvbuiltin "(_ extract     7  0)"}         #UInt32^to#UInt8 (n: #UInt32): #UInt8;
function {:bvbuiltin "(_ extract     15 0)"}         #UInt32^to#UInt16(n: #UInt32): #UInt16;
function {:bvbuiltin "(_ zero_extend 32  )"}         #UInt32^to#UInt64(n: #UInt32): #UInt64;
function {:builtin   "(_ to_fp_unsigned 11 53) RNE"} #UInt32^to#Double(n: #UInt32): #Double;

function {:bvbuiltin "(_ extract     7  0)"}         #UInt64^to#Int8  (n: #UInt64): #Int8;
function {:bvbuiltin "(_ extract     15 0)"}         #UInt64^to#Int16 (n: #UInt64): #Int16;
function {:bvbuiltin "(_ extract     31 0)"}         #UInt64^to#Int32 (n: #UInt64): #Int32;
function                                             #UInt64^to#Int64 (n: #UInt64): #Int64 { n }
function {:bvbuiltin "(_ extract     7  0)"}         #UInt64^to#UInt8 (n: #UInt64): #UInt8;
function {:bvbuiltin "(_ extract     15 0)"}         #UInt64^to#UInt16(n: #UInt64): #UInt16;
function {:bvbuiltin "(_ extract     31 0)"}         #UInt64^to#UInt32(n: #UInt64): #UInt32;
function {:builtin   "(_ to_fp_unsigned 11 53) RNE"} #UInt64^to#Double(n: #UInt64): #Double;

// FIXME(spinda): These floating-point conversions are unspecified when the
// input is out of range.
function {:builtin   "(_ fp.to_sbv   8 ) RNE"}       #Double^to#Int8  (n: #Double): #Int8;
function {:builtin   "(_ fp.to_sbv   16) RNE"}       #Double^to#Int16 (n: #Double): #Int16;
function {:builtin   "(_ fp.to_sbv   32) RNE"}       #Double^to#Int32 (n: #Double): #Int32;
function {:builtin   "(_ fp.to_sbv   64) RNE"}       #Double^to#Int64 (n: #Double): #Int64;
function {:builtin   "(_ fp.to_ubv   8 ) RNE"}       #Double^to#UInt8 (n: #Double): #UInt8;
function {:builtin   "(_ fp.to_ubv   16) RNE"}       #Double^to#UInt16(n: #Double): #UInt16;
function {:builtin   "(_ fp.to_ubv   32) RNE"}       #Double^to#UInt32(n: #Double): #UInt32;
function {:builtin   "(_ fp.to_ubv   64) RNE"}       #Double^to#UInt64(n: #Double): #UInt64;

function {:bvbuiltin "bvneg"}      #Int8^negate  (n: #Int8):              #Int8;
function {:bvbuiltin "bvadd"}      #Int8^add     (x: #Int8, y: #Int8):   #Int8;
function {:bvbuiltin "bvsub"}      #Int8^sub     (x: #Int8, y: #Int8):   #Int8;
function {:bvbuiltin "bvmul"}      #Int8^mul     (x: #Int8, y: #Int8):   #Int8;
function {:bvbuiltin "bvsdiv"}     #Int8^div     (x: #Int8, y: #Int8):   #Int8;
function {:bvbuiltin "bvsrem"}     #Int8^mod     (x: #Int8, y: #Int8):   #Int8;
function {:bvbuiltin "bvsle"}      #Int8^lte     (a: #Int8, y: #Int8):   #Bool;
function {:bvbuiltin "bvsge"}      #Int8^gte     (a: #Int8, y: #Int8):   #Bool;
function {:bvbuiltin "bvslt"}      #Int8^lt      (a: #Int8, y: #Int8):   #Bool;
function {:bvbuiltin "bvsgt"}      #Int8^gt      (a: #Int8, y: #Int8):   #Bool;
function {:bvbuiltin "bvnot"}      #Int8^bitNot  (x: #Int8):              #Int8;
function {:bvbuiltin "bvor"}       #Int8^bitOr   (a: #Int8, y: #Int8):   #Int8;
function {:bvbuiltin "bvand"}      #Int8^bitAnd  (a: #Int8, y: #Int8):   #Int8;
function {:bvbuiltin "bvxor"}      #Int8^xor     (a: #Int8, y: #Int8):   #Int8;
function {:bvbuiltin "bvshl"}      #Int8^shl     (a: #Int8, y: #Int8):   #Int8;
function {:bvbuiltin "bvlshr"}     #Int8^shr     (a: #Int8, y: #Int8):   #Int8;

function {:bvbuiltin "bvneg"}      #Int16^negate (n: #Int16):              #Int16;
function {:bvbuiltin "bvadd"}      #Int16^add    (x: #Int16, y: #Int16):   #Int16;
function {:bvbuiltin "bvsub"}      #Int16^sub    (x: #Int16, y: #Int16):   #Int16;
function {:bvbuiltin "bvmul"}      #Int16^mul    (x: #Int16, y: #Int16):   #Int16;
function {:bvbuiltin "bvsdiv"}     #Int16^div    (x: #Int16, y: #Int16):   #Int16;
function {:bvbuiltin "bvsrem"}     #Int16^mod    (x: #Int16, y: #Int16):   #Int16;
function {:bvbuiltin "bvsle"}      #Int16^lte    (a: #Int16, y: #Int16):   #Bool;
function {:bvbuiltin "bvsge"}      #Int16^gte    (a: #Int16, y: #Int16):   #Bool;
function {:bvbuiltin "bvslt"}      #Int16^lt     (a: #Int16, y: #Int16):   #Bool;
function {:bvbuiltin "bvsgt"}      #Int16^gt     (a: #Int16, y: #Int16):   #Bool;
function {:bvbuiltin "bvnot"}      #Int16^bitNot (x: #Int16):              #Int16;
function {:bvbuiltin "bvor"}       #Int16^bitOr  (a: #Int16, y: #Int16):   #Int16;
function {:bvbuiltin "bvand"}      #Int16^bitAnd (a: #Int16, y: #Int16):   #Int16;
function {:bvbuiltin "bvxor"}      #Int16^xor    (a: #Int16, y: #Int16):   #Int16;
function {:bvbuiltin "bvshl"}      #Int16^shl    (a: #Int16, y: #Int16):   #Int16;
function {:bvbuiltin "bvlshr"}     #Int16^shr    (a: #Int16, y: #Int16):   #Int16;

function {:bvbuiltin "bvneg"}      #Int32^negate (n: #Int32):              #Int32;
function {:bvbuiltin "bvadd"}      #Int32^add    (x: #Int32, y: #Int32):   #Int32;
function {:bvbuiltin "bvsub"}      #Int32^sub    (x: #Int32, y: #Int32):   #Int32;
function {:bvbuiltin "bvmul"}      #Int32^mul    (x: #Int32, y: #Int32):   #Int32;
function {:bvbuiltin "bvsdiv"}     #Int32^div    (x: #Int32, y: #Int32):   #Int32;
function {:bvbuiltin "bvsrem"}     #Int32^mod    (x: #Int32, y: #Int32):   #Int32;
function {:bvbuiltin "bvsle"}      #Int32^lte    (a: #Int32, y: #Int32):   #Bool;
function {:bvbuiltin "bvsge"}      #Int32^gte    (a: #Int32, y: #Int32):   #Bool;
function {:bvbuiltin "bvslt"}      #Int32^lt     (a: #Int32, y: #Int32):   #Bool;
function {:bvbuiltin "bvsgt"}      #Int32^gt     (a: #Int32, y: #Int32):   #Bool;
function {:bvbuiltin "bvnot"}      #Int32^bitNot (x: #Int32):              #Int32;
function {:bvbuiltin "bvor"}       #Int32^bitOr  (a: #Int32, y: #Int32):   #Int32;
function {:bvbuiltin "bvand"}      #Int32^bitAnd (a: #Int32, y: #Int32):   #Int32;
function {:bvbuiltin "bvxor"}      #Int32^xor    (a: #Int32, y: #Int32):   #Int32;
function {:bvbuiltin "bvshl"}      #Int32^shl    (a: #Int32, y: #Int32):   #Int32;
function {:bvbuiltin "bvlshr"}     #Int32^shr    (a: #Int32, y: #Int32):   #Int32;

function {:bvbuiltin "bvneg"}      #Int64^negate (n: #Int64):              #Int64;
function {:bvbuiltin "bvadd"}      #Int64^add    (x: #Int64, y: #Int64):   #Int64;
function {:bvbuiltin "bvsub"}      #Int64^sub    (x: #Int64, y: #Int64):   #Int64;
function {:bvbuiltin "bvmul"}      #Int64^mul    (x: #Int64, y: #Int64):   #Int64;
function {:bvbuiltin "bvsdiv"}     #Int64^div    (x: #Int64, y: #Int64):   #Int64;
function {:bvbuiltin "bvsrem"}     #Int64^mod    (x: #Int64, y: #Int64):   #Int64;
function {:bvbuiltin "bvsle"}      #Int64^lte    (a: #Int64, y: #Int64):   #Bool;
function {:bvbuiltin "bvsge"}      #Int64^gte    (a: #Int64, y: #Int64):   #Bool;
function {:bvbuiltin "bvslt"}      #Int64^lt     (a: #Int64, y: #Int64):   #Bool;
function {:bvbuiltin "bvsgt"}      #Int64^gt     (a: #Int64, y: #Int64):   #Bool;
function {:bvbuiltin "bvnot"}      #Int64^bitNot (x: #Int64):              #Int64;
function {:bvbuiltin "bvor"}       #Int64^bitOr  (a: #Int64, y: #Int64):   #Int64;
function {:bvbuiltin "bvand"}      #Int64^bitAnd (a: #Int64, y: #Int64):   #Int64;
function {:bvbuiltin "bvxor"}      #Int64^xor    (a: #Int64, y: #Int64):   #Int64;
function {:bvbuiltin "bvshl"}      #Int64^shl    (a: #Int64, y: #Int64):   #Int64;
function {:bvbuiltin "bvlshr"}     #Int64^shr    (a: #Int64, y: #Int64):   #Int64;

function {:bvbuiltin "bvneg"}      #UInt8^negate (n: #UInt8):             #UInt8;
function {:bvbuiltin "bvadd"}      #UInt8^add    (x: #UInt8, y: #UInt8): #UInt8;
function {:bvbuiltin "bvsub"}      #UInt8^sub    (x: #UInt8, y: #UInt8): #UInt8;
function {:bvbuiltin "bvmul"}      #UInt8^mul    (x: #UInt8, y: #UInt8): #UInt8;
function {:bvbuiltin "bvudiv"}     #UInt8^div    (x: #UInt8, y: #UInt8): #UInt8;
function {:bvbuiltin "bvurem"}     #UInt8^mod    (x: #UInt8, y: #UInt8): #UInt8;
function {:bvbuiltin "bvule"}      #UInt8^lte    (a: #UInt8, y: #UInt8): #Bool;
function {:bvbuiltin "bvuge"}      #UInt8^gte    (a: #UInt8, y: #UInt8): #Bool;
function {:bvbuiltin "bvult"}      #UInt8^lt     (a: #UInt8, y: #UInt8): #Bool;
function {:bvbuiltin "bvugt"}      #UInt8^gt     (a: #UInt8, y: #UInt8): #Bool;
function {:bvbuiltin "bvnot"}      #UInt8^bitNot (x: #UInt8):             #UInt8;
function {:bvbuiltin "bvor"}       #UInt8^bitOr  (a: #UInt8, y: #UInt8): #UInt8;
function {:bvbuiltin "bvand"}      #UInt8^bitAnd (a: #UInt8, y: #UInt8): #UInt8;
function {:bvbuiltin "bvxor"}      #UInt8^xor    (a: #UInt8, y: #UInt8): #UInt8;
function {:bvbuiltin "bvshl"}      #UInt8^shl    (a: #UInt8, y: #UInt8): #UInt8;
function {:bvbuiltin "bvlshr"}     #UInt8^shr    (a: #UInt8, y: #UInt8): #UInt8;

function {:bvbuiltin "bvneg"}      #UInt16^negate(n: #UInt16):             #UInt16;
function {:bvbuiltin "bvadd"}      #UInt16^add   (x: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvsub"}      #UInt16^sub   (x: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvmul"}      #UInt16^mul   (x: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvudiv"}     #UInt16^div   (x: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvurem"}     #UInt16^mod   (x: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvule"}      #UInt16^lte   (a: #UInt16, y: #UInt16): #Bool;
function {:bvbuiltin "bvuge"}      #UInt16^gte   (a: #UInt16, y: #UInt16): #Bool;
function {:bvbuiltin "bvult"}      #UInt16^lt    (a: #UInt16, y: #UInt16): #Bool;
function {:bvbuiltin "bvugt"}      #UInt16^gt    (a: #UInt16, y: #UInt16): #Bool;
function {:bvbuiltin "bvnot"}      #UInt16^bitNot(x: #UInt16):             #UInt16;
function {:bvbuiltin "bvor"}       #UInt16^bitOr (a: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvand"}      #UInt16^bitAnd(a: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvxor"}      #UInt16^xor   (a: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvshl"}      #UInt16^shl   (a: #UInt16, y: #UInt16): #UInt16;
function {:bvbuiltin "bvlshr"}     #UInt16^shr   (a: #UInt16, y: #UInt16): #UInt16;

function {:bvbuiltin "bvneg"}      #UInt32^negate(n: #UInt32):             #UInt32;
function {:bvbuiltin "bvadd"}      #UInt32^add   (x: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvsub"}      #UInt32^sub   (x: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvmul"}      #UInt32^mul   (x: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvudiv"}     #UInt32^div   (x: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvurem"}     #UInt32^mod   (x: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvule"}      #UInt32^lte   (a: #UInt32, y: #UInt32): #Bool;
function {:bvbuiltin "bvuge"}      #UInt32^gte   (a: #UInt32, y: #UInt32): #Bool;
function {:bvbuiltin "bvult"}      #UInt32^lt    (a: #UInt32, y: #UInt32): #Bool;
function {:bvbuiltin "bvugt"}      #UInt32^gt    (a: #UInt32, y: #UInt32): #Bool;
function {:bvbuiltin "bvnot"}      #UInt32^bitNot(x: #UInt32):             #UInt32;
function {:bvbuiltin "bvor"}       #UInt32^bitOr (a: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvand"}      #UInt32^bitAnd(a: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvxor"}      #UInt32^xor   (a: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvshl"}      #UInt32^shl   (a: #UInt32, y: #UInt32): #UInt32;
function {:bvbuiltin "bvlshr"}     #UInt32^shr   (a: #UInt32, y: #UInt32): #UInt32;

function {:bvbuiltin "bvneg"}      #UInt64^negate(n: #UInt64):             #UInt64;
function {:bvbuiltin "bvadd"}      #UInt64^add   (x: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvsub"}      #UInt64^sub   (x: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvmul"}      #UInt64^mul   (x: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvudiv"}     #UInt64^div   (x: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvurem"}     #UInt64^mod   (x: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvule"}      #UInt64^lte   (a: #UInt64, y: #UInt64): #Bool;
function {:bvbuiltin "bvuge"}      #UInt64^gte   (a: #UInt64, y: #UInt64): #Bool;
function {:bvbuiltin "bvult"}      #UInt64^lt    (a: #UInt64, y: #UInt64): #Bool;
function {:bvbuiltin "bvugt"}      #UInt64^gt    (a: #UInt64, y: #UInt64): #Bool;
function {:bvbuiltin "bvnot"}      #UInt64^bitNot(x: #UInt64):             #UInt32;
function {:bvbuiltin "bvor"}       #UInt64^bitOr (a: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvand"}      #UInt64^bitAnd(a: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvxor"}      #UInt64^xor   (a: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvshl"}      #UInt64^shl   (a: #UInt64, y: #UInt64): #UInt64;
function {:bvbuiltin "bvlshr"}     #UInt64^shr   (a: #UInt64, y: #UInt64): #UInt64;

function {:bvbuiltin "fp.neg"}     #Double^negate(n: #Double):             #Double;
function {:bvbuiltin "fp.add RNE"} #Double^add   (x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.sub RNE"} #Double^sub   (x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.mul RNE"} #Double^mul   (x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.div RNE"} #Double^div   (x: #Double, y: #Double): #Double;
function {:bvbuiltin "fp.eq"}      #Double^eq    (x: #Double, y: #Double): #Bool;
function {:bvbuiltin "fp.leq"}     #Double^lte   (x: #Double, y: #Double): #Bool;
function {:bvbuiltin "fp.geq"}     #Double^gte   (x: #Double, y: #Double): #Bool;
function {:bvbuiltin "fp.lt"}      #Double^lt    (x: #Double, y: #Double): #Bool;
function {:bvbuiltin "fp.gt"}      #Double^gt    (x: #Double, y: #Double): #Bool;

type #Map k v = [k]v;

function {:builtin "MapConst"} #Map~const<k, v>(value: v): #Map k v;

function {:inline} #Map~get<k, v>(map: #Map k v, key: k): v {
  map[key]
}

function {:inline} #Map~set<k, v>(map: #Map k v, key: k, value: v): #Map k v {
  map[key := value]
}

type #Set a = #Map a #Bool;

function {:inline} #Set~empty<a>(): #Set a {
  #Map~const(false)
}

function {:inline} #Set~all<a>(): #Set a {
  #Map~const(true)
}

function {:inline} #Set~contains<a>(set: #Set a, value: a): #Bool {
  #Map~get(set, value)
}

function {:inline} #Set~add<a>(set: #Set a, value: a): #Set a {
  #Map~set(set, value, true)
}

function {:inline} #Set~remove<a>(set: #Set a, value: a): #Set a {
  #Map~set(set, value, false)
}

function {:builtin "MapOr"} #Set~union<a>(lhs: #Set a, rhs: #Set a): #Set a;

function {:builtin "MapAnd"} #Set~intersect<a>(lhs: #Set a, rhs: #Set a): #Set a;

function {:builtin "MapNot"} #Set~complement<a>(set: #Set a): #Set a;

function {:inline} #Set~difference<a>(lhs: #Set a, rhs: #Set a): #Set a
{
  #Set~intersect(lhs, #Set~complement(rhs))
}

// Impls for Cachet's prelude...

const #Double~INFINITY: #Double;
axiom #Double~INFINITY == 0+oo53e11;

const #Double~NEG_INFINITY: #Double;
axiom #Double~NEG_INFINITY == 0-oo53e11;

procedure boogie_si_record_bv32(x:bv32);
procedure #Int32~print(d: #Int32) {
  call {:cexpr "Int32"} boogie_si_record_bv32(d);
}
procedure #UInt32~print(d: #UInt32) {
  call {:cexpr "UInt32"} boogie_si_record_bv32(d);
}

function {:bvbuiltin "bv2nat"} bv2nat31(i: bv31): int;
function {:bvbuiltin "(_ extract 31 31)"} bvsign32(i: bv32): bv1;
function {:bvbuiltin "(_ extract 30 0)"} bvmag32(i: bv32): bv31;
function bv2int32(i: bv32): int {
  // The form of this expression is very specific, so that z3 will recognize it
  // as a signed bitvector-to-int conversion and optimize accordingly.
  // Unfortunately, SMT-LIB doesn't provide us with a signed bv2int to call
  // explicitly. Technically, there *is* a "bv2int", but it's an alias for
  // bv2nat.
  if 1bv1 == bvsign32(i)
    then bv2nat31(bvmag32(i)) - 2147483648
    else bv2nat31(bvmag32(i))
}

procedure {:inline 1} #Int32~checkedAdd(x: #Int32, y: #Int32)
  returns (sum: #Int32, ret: #Bool)
{
  var intSum: int;

  sum := #Int32^add(x, y);

  intSum := bv2int32(x) + bv2int32(y);
  ret := intSum >= -2147483648 && intSum <= 2147483647;
}

procedure boogie_si_record_bv64(x:#UInt64);
procedure #Double~print(d: #Double) {
  call {:cexpr "Double"} boogie_si_record_bv64(#Double~bits(d));
}

function {:builtin "fp.isInfinite"} #Double~isInfinite(n: #Double): #Bool;
function {:builtin "fp.isNaN"} #Double~is_nan(n: #Double): #Bool;
function {:bvbuiltin "fp.roundToIntegral RTP"} #Double~ceil(x: #Double): #Double;
function {:bvbuiltin "fp.abs"} #Double~abs(x: #Double): #Double;

// From <https://smtlib.cs.uiowa.edu/theories-FloatingPoint.shtml>:
//
// > There is no function for converting from (_ FloatingPoint eb sb) to the
// > corresponding IEEE 754-2008 binary format, as a bit vector (_ BitVec m)
// > with > m = eb + sb, because (_ NaN eb sb) has multiple, well-defined
// > representations. Instead, an encoding of the kind below is recommended,
// > where f is a term of sort (_ FloatingPoint eb sb):
// >
// > (declare-fun b () (_ BitVec m))
// > (assert (= ((_ to_fp eb sb) b) f))
function                              #Double~bits     (n: #Double): #UInt64;
function {:builtin "(_ to_fp 11 53)"} #Double~from_bits(n: #UInt64): #Double;
axiom (forall d: #Double :: #Double~from_bits(#Double~bits(d)) == d);

// ... end prelude ...
