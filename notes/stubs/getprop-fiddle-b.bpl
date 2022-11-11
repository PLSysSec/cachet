type #ValueReg = #Reg;

procedure #ValueReg~scratchReg($valueReg: #ValueReg)
  returns (reg: #Reg)
{
  reg := $valueReg;
}

var #MASM~regs: #Map #Reg #RegData;

procedure #MASM~getData($reg: #Reg)
  returns (ret: #RegData)
{
  ret := #Map~get(#MASM~regs, $reg);
}

procedure #MASM~setData($reg: #Reg, $data: #RegData)
  modifies #MASM~regs;
{
  #MASM~regs := #Map~set(#MASM~regs, $reg, $data);
}

procedure #MASM~getValue($valueReg: #ValueReg)
  returns (ret: #Value)
{
  var tmp'0: #RegData;
  call tmp'0 := #MASM~getData($valueReg);
  call ret := #RegData~toValue(tmp'0);
}

procedure #MASM~setValue($valueReg: #ValueReg, $value: #Value)
  modifies #MASM~regs;
{
  var tmp'0: #RegData;
  call tmp'0 := #RegData~fromValue($value);
  call #MASM~setData($valueReg, tmp'0);
}

var #CacheIR~knownOperandIds: #Set #UInt16;

var #CacheIR~operandLocations: #Map #UInt16 #OperandLocation;

procedure #CacheIR~defineTypedId($typedId: #TypedId)
    returns (ret: #Reg)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
    var $id'0: #UInt16;
    var $loc'0: #OperandLocation;
    var $loc'1: #OperandLocation;
    var $locKind'0: #OperandLocationKind;
    
    $id'0 := #OperandId~id(#TypedId^to#OperandId($typedId));
    $loc'0 := #Map~get(#CacheIR~operandLocations, $id'0);
    $locKind'0 := #OperandLocation~kind($loc'0);
    assert $locKind'0 == #OperandLocationKind^Variant~Uninitialized();
    
    call ret := #CacheIR~allocateReg();
    call $loc'1 := #OperandLocation~setPayloadReg(ret, #TypedId~type($typedId));
    #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $id'0, $loc'1);
}

procedure #CacheIR~defineValueId($valueId: #ValueId)
    returns (ret: #ValueReg)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
    var $id'0: #UInt16;
    var $loc'0: #OperandLocation;
    var $loc'1: #OperandLocation;
    var $locKind'0: #OperandLocationKind;
    
    $id'0 := #OperandId~id(#ValueId^to#OperandId($valueId));
    $loc'0 := #Map~get(#CacheIR~operandLocations, $id'0);
    $locKind'0 := #OperandLocation~kind($loc'0);
    assert $locKind'0 == #OperandLocationKind^Variant~Uninitialized();
    
    call ret := #CacheIR~allocateValueReg();
    call $loc'1 := #OperandLocation~setValueReg(ret);
    #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $id'0, $loc'1);
}

procedure #CacheIR~getOperandLocation($operandId: #OperandId)
    returns (loc: #OperandLocation)
{
    loc := #Map~get(#CacheIR~operandLocations, #OperandId~id($operandId));
}

procedure #CacheIR~setOperandLocation($operandId: #OperandId, $loc: #OperandLocation)
    modifies #CacheIR~operandLocations;
{
    #CacheIR~operandLocations :=
        #Map~set(#CacheIR~operandLocations, #OperandId~id($operandId), $loc);
}

procedure #initOperandId($operandId: #OperandId)
    modifies #CacheIR~operandLocations, #CacheIR~knownOperandIds;
{
    var $id'0: #UInt16;
    var tmp'0: #Bool;
    var $loc'0: #OperandLocation;

    $id'0 := #OperandId~id($operandId);
    tmp'0 := #Set~contains(#CacheIR~knownOperandIds, $id'0);
    assume !tmp'0;
    #CacheIR~knownOperandIds := #Set~add(#CacheIR~knownOperandIds, $id'0);

    call $loc'0 := #OperandLocation~newUninitialized();
    #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $id'0, $loc'0);
}

var #CacheIR~allocatedRegs: #Set #Reg;

procedure #CacheIR~allocateValueReg()
  returns (ret: #ValueReg)
  modifies #CacheIR~allocatedRegs;
{
  call ret := #CacheIR~allocateReg();
}

procedure #CacheIR~releaseValueReg($valueReg: #ValueReg)
  modifies #CacheIR~allocatedRegs;
{
  call #CacheIR~releaseReg($valueReg);
}

procedure #CacheIR~allocateReg()
  returns (ret: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  var $reg: #Reg;
  var tmp'0: #Bool;
  $reg := #CacheIR~allocateRegUnchecked(#CacheIR~allocatedRegs);
  tmp'0 := #Set~contains(#CacheIR~allocatedRegs, $reg);
  assume !tmp'0;
  #CacheIR~allocatedRegs := #Set~add(#CacheIR~allocatedRegs, $reg);
  ret := $reg;
}

procedure #CacheIR~allocateKnownReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  var tmp'0: #Bool;
  tmp'0 := #Set~contains(#CacheIR~allocatedRegs, $reg);
  assume !tmp'0;
  #CacheIR~allocatedRegs := #Set~add(#CacheIR~allocatedRegs, $reg);
}

function #CacheIR~allocateRegUnchecked($allocatedRegs: #Set #Reg): #Reg;

procedure #CacheIR~releaseReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  #CacheIR~allocatedRegs := #Set~remove(#CacheIR~allocatedRegs, $reg);
}

procedure #initInputValueId($valueId: #ValueId)
  modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $valueReg'0: #ValueReg;
  var $data'1: #RegData;
  var tmp'2: #Bool;
  call $valueReg'0 := #CacheIR~defineValueId($valueId);
  call $data'1 := #MASM~getData($valueReg'0);
  call tmp'2 := #RegData~isValue($data'1);
  assume tmp'2;
}

// ... begin prelude ...

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
function {:bvbuiltin "bvnot"}      #UInt64^bitNot(x: #UInt32):             #UInt32;
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

procedure boogie_si_record_bv64(x:#UInt64);
procedure #Double~print(d: #Double) {
  call {:cexpr "Double"} boogie_si_record_bv64(#Double~bits(d));
}
function {:builtin "fp.isInfinite"} #Double~isInfinite(n: #Double): #Bool;
function {:builtin "fp.isNaN"} #Double~is_nan(n: #Double): #Bool;
function {:bvbuiltin "fp.roundToIntegral RTP"} #Double~ceil(x: #Double): #Double;
function {:bvbuiltin "fp.abs"} #Double~abs(x: #Double): #Double;

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
function                              #Double~bits     (n: #Double): #UInt64;
function {:builtin "(_ to_fp 11 53)"} #Double~from_bits(n: #UInt64): #Double;
axiom (forall d: #Double :: #Double~from_bits(#Double~bits(d)) == d);

// ... end prelude ...


type {:datatype} #RegDataType;

function {:constructor} #RegDataType^Variant~Value(): #RegDataType;

function {:constructor} #RegDataType^Variant~UnboxedValue(): #RegDataType;

function {:constructor} #RegDataType^Variant~NativeObjectSlots(): #RegDataType;

function {:constructor} #RegDataType^Variant~NativeObjectElements(): #RegDataType;

function {:constructor} #RegDataType^Variant~TypeTag(): #RegDataType;

function {:constructor} #RegDataType^Variant~TaggedProto(): #RegDataType;

type {:datatype} #ValueType;

function {:constructor} #ValueType^Variant~Double(): #ValueType;

function {:constructor} #ValueType^Variant~Int32(): #ValueType;

function {:constructor} #ValueType^Variant~Bool(): #ValueType;

function {:constructor} #ValueType^Variant~Undefined(): #ValueType;

function {:constructor} #ValueType^Variant~Null(): #ValueType;

function {:constructor} #ValueType^Variant~Magic(): #ValueType;

function {:constructor} #ValueType^Variant~String(): #ValueType;

function {:constructor} #ValueType^Variant~Symbol(): #ValueType;

function {:constructor} #ValueType^Variant~PrivateGCThing(): #ValueType;

function {:constructor} #ValueType^Variant~BigInt(): #ValueType;

function {:constructor} #ValueType^Variant~Object(): #ValueType;

function {:constructor} #ValueType^Variant~Unknown(): #ValueType;

type {:datatype} #ProtoTag;

function {:constructor} #ProtoTag^Variant~Null(): #ProtoTag;

function {:constructor} #ProtoTag^Variant~Lazy(): #ProtoTag;

function {:constructor} #ProtoTag^Variant~Object(): #ProtoTag;

type {:datatype} #Condition;

function {:constructor} #Condition^Variant~Equal(): #Condition;

function {:constructor} #Condition^Variant~NotEqual(): #Condition;

function {:constructor} #Condition^Variant~Overflow(): #Condition;

function {:constructor} #Condition^Variant~Zero(): #Condition;

function {:constructor} #Condition^Variant~NonZero(): #Condition;

function {:constructor} #Condition^Variant~Signed(): #Condition;

function {:constructor} #Condition^Variant~NotSigned(): #Condition;

function {:constructor} #Condition^Variant~GreaterThan(): #Condition;

function {:constructor} #Condition^Variant~LessThan(): #Condition;

type {:datatype} #Scale;

function {:constructor} #Scale^Variant~TimesOne(): #Scale;

function {:constructor} #Scale^Variant~TimesTwo(): #Scale;

function {:constructor} #Scale^Variant~TimesFour(): #Scale;

function {:constructor} #Scale^Variant~TimesEight(): #Scale;

type {:datatype} #OperandLocationKind;

function {:constructor} #OperandLocationKind^Variant~Uninitialized(): #OperandLocationKind;

function {:constructor} #OperandLocationKind^Variant~PayloadReg(): #OperandLocationKind;

function {:constructor} #OperandLocationKind^Variant~ValueReg(): #OperandLocationKind;

type {:datatype} #GuardClassKind;

function {:constructor} #GuardClassKind^Variant~Array(): #GuardClassKind;

function {:constructor} #GuardClassKind^Variant~PlainObject(): #GuardClassKind;

function {:constructor} #GuardClassKind^Variant~ArrayBuffer(): #GuardClassKind;

function {:constructor} #GuardClassKind^Variant~SharedArrayBuffer(): #GuardClassKind;

function {:constructor} #GuardClassKind^Variant~DataView(): #GuardClassKind;

function {:constructor} #GuardClassKind^Variant~MappedArguments(): #GuardClassKind;

function {:constructor} #GuardClassKind^Variant~UnmappedArguments(): #GuardClassKind;

function {:constructor} #GuardClassKind^Variant~WindowProxy(): #GuardClassKind;

function {:constructor} #GuardClassKind^Variant~JSFunction(): #GuardClassKind;

function {:constructor} #GuardClassKind^Variant~Set(): #GuardClassKind;

function {:constructor} #GuardClassKind^Variant~Map(): #GuardClassKind;

type #Heap;

type #RegData;

type #Value;

type #Object;

type #NativeObject;

function #Object^to#NativeObject(in: #Object): #NativeObject;

function #NativeObject^to#Object(in: #NativeObject): #Object;

axiom (forall in: #NativeObject :: (#Object^to#NativeObject(#NativeObject^to#Object(in)) == in));

axiom (forall in: #Object :: (#NativeObject^to#Object(#Object^to#NativeObject(in)) == in));

type #NativeObjectSlots;

type #NativeObjectElements;

type #JSFunction;

function #NativeObject^to#JSFunction(in: #NativeObject): #JSFunction;

function #JSFunction^to#NativeObject(in: #JSFunction): #NativeObject;

axiom (forall in: #JSFunction :: (#NativeObject^to#JSFunction(#JSFunction^to#NativeObject(in)) == in));

axiom (forall in: #NativeObject :: (#JSFunction^to#NativeObject(#NativeObject^to#JSFunction(in)) == in));

type #ArrayObject;

function #NativeObject^to#ArrayObject(in: #NativeObject): #ArrayObject;

function #ArrayObject^to#NativeObject(in: #ArrayObject): #NativeObject;

axiom (forall in: #ArrayObject :: (#NativeObject^to#ArrayObject(#ArrayObject^to#NativeObject(in)) == in));

axiom (forall in: #NativeObject :: (#ArrayObject^to#NativeObject(#NativeObject^to#ArrayObject(in)) == in));

type #PlainObject;

function #NativeObject^to#PlainObject(in: #NativeObject): #PlainObject;

function #PlainObject^to#NativeObject(in: #PlainObject): #NativeObject;

axiom (forall in: #PlainObject :: (#NativeObject^to#PlainObject(#PlainObject^to#NativeObject(in)) == in));

axiom (forall in: #NativeObject :: (#PlainObject^to#NativeObject(#NativeObject^to#PlainObject(in)) == in));

type #ArrayBufferObjectMaybeShared;

function #NativeObject^to#ArrayBufferObjectMaybeShared(in: #NativeObject): #ArrayBufferObjectMaybeShared;

function #ArrayBufferObjectMaybeShared^to#NativeObject(in: #ArrayBufferObjectMaybeShared): #NativeObject;

axiom (forall in: #ArrayBufferObjectMaybeShared :: (#NativeObject^to#ArrayBufferObjectMaybeShared(#ArrayBufferObjectMaybeShared^to#NativeObject(in)) == in));

axiom (forall in: #NativeObject :: (#ArrayBufferObjectMaybeShared^to#NativeObject(#NativeObject^to#ArrayBufferObjectMaybeShared(in)) == in));

type #ArrayBufferObject;

function #ArrayBufferObjectMaybeShared^to#ArrayBufferObject(in: #ArrayBufferObjectMaybeShared): #ArrayBufferObject;

function #ArrayBufferObject^to#ArrayBufferObjectMaybeShared(in: #ArrayBufferObject): #ArrayBufferObjectMaybeShared;

axiom (forall in: #ArrayBufferObject :: (#ArrayBufferObjectMaybeShared^to#ArrayBufferObject(#ArrayBufferObject^to#ArrayBufferObjectMaybeShared(in)) == in));

axiom (forall in: #ArrayBufferObjectMaybeShared :: (#ArrayBufferObject^to#ArrayBufferObjectMaybeShared(#ArrayBufferObjectMaybeShared^to#ArrayBufferObject(in)) == in));

type #SharedArrayBufferObject;

function #ArrayBufferObjectMaybeShared^to#SharedArrayBufferObject(in: #ArrayBufferObjectMaybeShared): #SharedArrayBufferObject;

function #SharedArrayBufferObject^to#ArrayBufferObjectMaybeShared(in: #SharedArrayBufferObject): #ArrayBufferObjectMaybeShared;

axiom (forall in: #SharedArrayBufferObject :: (#ArrayBufferObjectMaybeShared^to#SharedArrayBufferObject(#SharedArrayBufferObject^to#ArrayBufferObjectMaybeShared(in)) == in));

axiom (forall in: #ArrayBufferObjectMaybeShared :: (#SharedArrayBufferObject^to#ArrayBufferObjectMaybeShared(#ArrayBufferObjectMaybeShared^to#SharedArrayBufferObject(in)) == in));

type #ArrayBufferViewObject;

function #NativeObject^to#ArrayBufferViewObject(in: #NativeObject): #ArrayBufferViewObject;

function #ArrayBufferViewObject^to#NativeObject(in: #ArrayBufferViewObject): #NativeObject;

axiom (forall in: #ArrayBufferViewObject :: (#NativeObject^to#ArrayBufferViewObject(#ArrayBufferViewObject^to#NativeObject(in)) == in));

axiom (forall in: #NativeObject :: (#ArrayBufferViewObject^to#NativeObject(#NativeObject^to#ArrayBufferViewObject(in)) == in));

type #DataViewObject;

function #ArrayBufferViewObject^to#DataViewObject(in: #ArrayBufferViewObject): #DataViewObject;

function #DataViewObject^to#ArrayBufferViewObject(in: #DataViewObject): #ArrayBufferViewObject;

axiom (forall in: #DataViewObject :: (#ArrayBufferViewObject^to#DataViewObject(#DataViewObject^to#ArrayBufferViewObject(in)) == in));

axiom (forall in: #ArrayBufferViewObject :: (#DataViewObject^to#ArrayBufferViewObject(#ArrayBufferViewObject^to#DataViewObject(in)) == in));

type #ArgumentsObject;

function #NativeObject^to#ArgumentsObject(in: #NativeObject): #ArgumentsObject;

function #ArgumentsObject^to#NativeObject(in: #ArgumentsObject): #NativeObject;

axiom (forall in: #ArgumentsObject :: (#NativeObject^to#ArgumentsObject(#ArgumentsObject^to#NativeObject(in)) == in));

axiom (forall in: #NativeObject :: (#ArgumentsObject^to#NativeObject(#NativeObject^to#ArgumentsObject(in)) == in));

type #UnmappedArgumentsObject;

function #ArgumentsObject^to#UnmappedArgumentsObject(in: #ArgumentsObject): #UnmappedArgumentsObject;

function #UnmappedArgumentsObject^to#ArgumentsObject(in: #UnmappedArgumentsObject): #ArgumentsObject;

axiom (forall in: #UnmappedArgumentsObject :: (#ArgumentsObject^to#UnmappedArgumentsObject(#UnmappedArgumentsObject^to#ArgumentsObject(in)) == in));

axiom (forall in: #ArgumentsObject :: (#UnmappedArgumentsObject^to#ArgumentsObject(#ArgumentsObject^to#UnmappedArgumentsObject(in)) == in));

type #MappedArgumentsObject;

function #ArgumentsObject^to#MappedArgumentsObject(in: #ArgumentsObject): #MappedArgumentsObject;

function #MappedArgumentsObject^to#ArgumentsObject(in: #MappedArgumentsObject): #ArgumentsObject;

axiom (forall in: #MappedArgumentsObject :: (#ArgumentsObject^to#MappedArgumentsObject(#MappedArgumentsObject^to#ArgumentsObject(in)) == in));

axiom (forall in: #ArgumentsObject :: (#MappedArgumentsObject^to#ArgumentsObject(#ArgumentsObject^to#MappedArgumentsObject(in)) == in));

type #SetObject;

function #NativeObject^to#SetObject(in: #NativeObject): #SetObject;

function #SetObject^to#NativeObject(in: #SetObject): #NativeObject;

axiom (forall in: #SetObject :: (#NativeObject^to#SetObject(#SetObject^to#NativeObject(in)) == in));

axiom (forall in: #NativeObject :: (#SetObject^to#NativeObject(#NativeObject^to#SetObject(in)) == in));

type #MapObject;

function #NativeObject^to#MapObject(in: #NativeObject): #MapObject;

function #MapObject^to#NativeObject(in: #MapObject): #NativeObject;

axiom (forall in: #MapObject :: (#NativeObject^to#MapObject(#MapObject^to#NativeObject(in)) == in));

axiom (forall in: #NativeObject :: (#MapObject^to#NativeObject(#NativeObject^to#MapObject(in)) == in));

type #BaseShape;

type #Shape;

type #Class;

type #String;

type #Symbol;

type #BigInt;

type #TaggedProto;

type #Reg;

type #Address;

function #Address^field~base(instance: #Address): #Reg;

function #Address^field~offset(instance: #Address): #Int32;

type #BaseIndex;

function #BaseIndex^field~base(instance: #BaseIndex): #Reg;

function #BaseIndex^field~index(instance: #BaseIndex): #Reg;

function #BaseIndex^field~scale(instance: #BaseIndex): #Scale;

function #BaseIndex^field~offset(instance: #BaseIndex): #UInt32;

type #BaseValueIndex;

function #BaseValueIndex^field~inner(instance: #BaseValueIndex): #BaseIndex;

type #BaseObjectElementIndex;

function #BaseObjectElementIndex^field~inner(instance: #BaseObjectElementIndex): #BaseValueIndex;

type #BaseObjectSlotIndex;

function #BaseObjectSlotIndex^field~inner(instance: #BaseObjectSlotIndex): #BaseValueIndex;

type #Operand;

type #OperandLocation;

type #OperandId;

type #ValueId;

function #OperandId^to#ValueId(in: #OperandId): #ValueId;

function #ValueId^to#OperandId(in: #ValueId): #OperandId;

axiom (forall in: #ValueId :: (#OperandId^to#ValueId(#ValueId^to#OperandId(in)) == in));

axiom (forall in: #OperandId :: (#ValueId^to#OperandId(#OperandId^to#ValueId(in)) == in));

type #ObjectId;

function #OperandId^to#ObjectId(in: #OperandId): #ObjectId;

function #ObjectId^to#OperandId(in: #ObjectId): #OperandId;

axiom (forall in: #ObjectId :: (#OperandId^to#ObjectId(#ObjectId^to#OperandId(in)) == in));

axiom (forall in: #OperandId :: (#ObjectId^to#OperandId(#OperandId^to#ObjectId(in)) == in));

type #StringId;

function #OperandId^to#StringId(in: #OperandId): #StringId;

function #StringId^to#OperandId(in: #StringId): #OperandId;

axiom (forall in: #StringId :: (#OperandId^to#StringId(#StringId^to#OperandId(in)) == in));

axiom (forall in: #OperandId :: (#StringId^to#OperandId(#OperandId^to#StringId(in)) == in));

type #SymbolId;

function #OperandId^to#SymbolId(in: #OperandId): #SymbolId;

function #SymbolId^to#OperandId(in: #SymbolId): #OperandId;

axiom (forall in: #SymbolId :: (#OperandId^to#SymbolId(#SymbolId^to#OperandId(in)) == in));

axiom (forall in: #OperandId :: (#SymbolId^to#OperandId(#OperandId^to#SymbolId(in)) == in));

type #BoolId;

function #OperandId^to#BoolId(in: #OperandId): #BoolId;

function #BoolId^to#OperandId(in: #BoolId): #OperandId;

axiom (forall in: #BoolId :: (#OperandId^to#BoolId(#BoolId^to#OperandId(in)) == in));

axiom (forall in: #OperandId :: (#BoolId^to#OperandId(#OperandId^to#BoolId(in)) == in));

type #Int32Id;

function #OperandId^to#Int32Id(in: #OperandId): #Int32Id;

function #Int32Id^to#OperandId(in: #Int32Id): #OperandId;

axiom (forall in: #Int32Id :: (#OperandId^to#Int32Id(#Int32Id^to#OperandId(in)) == in));

axiom (forall in: #OperandId :: (#Int32Id^to#OperandId(#OperandId^to#Int32Id(in)) == in));

type #NumberId;

function #ValueId^to#NumberId(in: #ValueId): #NumberId;

function #NumberId^to#ValueId(in: #NumberId): #ValueId;

axiom (forall in: #NumberId :: (#ValueId^to#NumberId(#NumberId^to#ValueId(in)) == in));

axiom (forall in: #ValueId :: (#NumberId^to#ValueId(#ValueId^to#NumberId(in)) == in));

type #BigIntId;

function #OperandId^to#BigIntId(in: #OperandId): #BigIntId;

function #BigIntId^to#OperandId(in: #BigIntId): #OperandId;

axiom (forall in: #BigIntId :: (#OperandId^to#BigIntId(#BigIntId^to#OperandId(in)) == in));

axiom (forall in: #OperandId :: (#BigIntId^to#OperandId(#OperandId^to#BigIntId(in)) == in));

type #ValueTagId;

function #OperandId^to#ValueTagId(in: #OperandId): #ValueTagId;

function #ValueTagId^to#OperandId(in: #ValueTagId): #OperandId;

axiom (forall in: #ValueTagId :: (#OperandId^to#ValueTagId(#ValueTagId^to#OperandId(in)) == in));

axiom (forall in: #OperandId :: (#ValueTagId^to#OperandId(#OperandId^to#ValueTagId(in)) == in));

type #IntPtrId;

function #OperandId^to#IntPtrId(in: #OperandId): #IntPtrId;

function #IntPtrId^to#OperandId(in: #IntPtrId): #OperandId;

axiom (forall in: #IntPtrId :: (#OperandId^to#IntPtrId(#IntPtrId^to#OperandId(in)) == in));

axiom (forall in: #OperandId :: (#IntPtrId^to#OperandId(#OperandId^to#IntPtrId(in)) == in));

type #RawId;

function #OperandId^to#RawId(in: #OperandId): #RawId;

function #RawId^to#OperandId(in: #RawId): #OperandId;

axiom (forall in: #RawId :: (#OperandId^to#RawId(#RawId^to#OperandId(in)) == in));

axiom (forall in: #OperandId :: (#RawId^to#OperandId(#OperandId^to#RawId(in)) == in));

type #TypedId;

function #OperandId^to#TypedId(in: #OperandId): #TypedId;

function #TypedId^to#OperandId(in: #TypedId): #OperandId;

axiom (forall in: #TypedId :: (#OperandId^to#TypedId(#TypedId^to#OperandId(in)) == in));

axiom (forall in: #OperandId :: (#TypedId^to#OperandId(#OperandId^to#TypedId(in)) == in));

type #Int32Field;

type #IntPtrField;

type #ShapeField;

type #ClassField;

type #GetterSetterField;

type #ObjectField;

type #SymbolField;

type #StringField;

type #BaseScriptField;

type #IdField;

type #AllocSiteField;

type #Int64Field;

type #ValueField;

type {:datatype} #MASM^Op;

function {:constructor} #MASM^Op^Exit(): #MASM^Op;

var #MASM^emitPath: EmitPath;

function #MASM^ops(emitPath: EmitPath): #MASM^Op;

function #MASM^nextEmitPath(emitPath: EmitPath): EmitPath;

procedure {:inline 1} #MASM^step()
{
  #MASM^emitPath := #MASM^nextEmitPath(#MASM^emitPath);
}

procedure #MASM^emit(op: #MASM^Op, emitPath: EmitPath)
{
  assume (#MASM^ops(emitPath) == op);
  assume (#MASM^nextEmitPath(#MASM^emitPath) == emitPath);
  #MASM^emitPath := emitPath;
}

function #MASM^labelEmitPath(label: EmitPath): EmitPath;

procedure {:inline 1} #MASM^bind(label: EmitPath)
{
  assume #MASM^labelEmitPath(label) == #MASM^nextEmitPath(#MASM^emitPath);
}

procedure {:inline 1} #MASM^bindExit(label: EmitPath)
{
  assume #MASM^labelEmitPath(label) == NilEmitPath();
}

procedure {:inline 1} #MASM^goto(label: EmitPath)
{
  #MASM^emitPath := #MASM^labelEmitPath(label);
}

function #UInt16~max(): #UInt16{  65535bv16}

var #heap: #Heap;

function #Value~sizeOf(): #UInt64{  8bv64}

function #NativeObject~maxFixedSlots(): #UInt64{  16bv64}

function #NativeObject~sizeOf(): #UInt64{  24bv64}

function #NativeObject~offsetOfSlots(): #UInt64{  8bv64}

function #NativeObject~offsetOfElements(): #UInt64{  16bv64}

function #NativeObjectElements~offsetOfLength(): #Int32{  4294967292bv32}

function #NativeObjectElements~offsetOfInitializedLength(): #Int32{  4294967284bv32}

const #ArrayObject~rawClass: #Class;

const #PlainObject~rawClass: #Class;

const #ArrayBufferObject~rawClass: #Class;

const #SharedArrayBufferObject~rawClass: #Class;

const #DataViewObject~rawClass: #Class;

const #UnmappedArgumentsObject~rawClass: #Class;

const #MappedArgumentsObject~rawClass: #Class;

const #SetObject~rawClass: #Class;

const #MapObject~rawClass: #Class;

const #Class~rawFunctionClass: #Class;

const #Class~rawExtendedFunctionClass: #Class;

function #ValueScale(): #Scale{  #Scale^Variant~TimesEight()}

const #CacheIR~outputReg: #ValueReg;

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

function #RegData~typeOf($data: #RegData): #RegDataType;

procedure #RegData~isValue($data: #RegData)
  returns (ret: #Bool)
{
  var $tmp'v0: #RegDataType;
  
  $tmp'v0 := #RegData~typeOf($data);
  ret := ($tmp'v0 == #RegDataType^Variant~Value());
  return;
}

procedure #RegData~fromValue($value: #Value)
  returns (ret: #RegData)
{
  var $data'v0: #RegData;
  var $tmp'v1: #Value;
  var out'0: #Bool;
  
  $data'v0 := #RegData~fromValueUnchecked($value);
  call out'0 := #RegData~isValue($data'v0);
  assume out'0;
  $tmp'v1 := #RegData~toValueUnchecked($data'v0);
  assume ($tmp'v1 == $value);
  ret := $data'v0;
  return;
}

function #RegData~fromValueUnchecked($value: #Value): #RegData;

procedure #RegData~toValue($data: #RegData)
  returns (ret: #Value)
{
  var $value'v0: #Value;
  var $tmp'v1: #RegData;
  var out'0: #Bool;
  
  call out'0 := #RegData~isValue($data);
  assert out'0;
  $value'v0 := #RegData~toValueUnchecked($data);
  $tmp'v1 := #RegData~fromValueUnchecked($value'v0);
  assume ($tmp'v1 == $data);
  ret := $value'v0;
  return;
}

function #RegData~toValueUnchecked($data: #RegData): #Value;

procedure #RegData~isUnboxedValue($data: #RegData)
  returns (ret: #Bool)
{
  var $tmp'v0: #RegDataType;
  
  $tmp'v0 := #RegData~typeOf($data);
  ret := ($tmp'v0 == #RegDataType^Variant~UnboxedValue());
  return;
}

procedure #RegData~fromUnboxedValue($value: #Value)
  returns (ret: #RegData)
{
  var $data'v0: #RegData;
  var $tmp'v1: #Value;
  var out'0: #Bool;
  
  $data'v0 := #RegData~fromUnboxedValueUnchecked($value);
  call out'0 := #RegData~isUnboxedValue($data'v0);
  assume out'0;
  $tmp'v1 := #RegData~toUnboxedValueUnchecked($data'v0);
  assume ($tmp'v1 == $value);
  ret := $data'v0;
  return;
}

function #RegData~fromUnboxedValueUnchecked($value: #Value): #RegData;

procedure #RegData~toUnboxedValue($data: #RegData)
  returns (ret: #Value)
{
  var $value'v0: #Value;
  var $tmp'v1: #RegData;
  var out'0: #Bool;
  
  call out'0 := #RegData~isUnboxedValue($data);
  assert out'0;
  $value'v0 := #RegData~toUnboxedValueUnchecked($data);
  $tmp'v1 := #RegData~fromUnboxedValueUnchecked($value'v0);
  assume ($tmp'v1 == $data);
  ret := $value'v0;
  return;
}

function #RegData~toUnboxedValueUnchecked($data: #RegData): #Value;

procedure #RegData~isNativeObjectSlots($data: #RegData)
  returns (ret: #Bool)
{
  var $tmp'v0: #RegDataType;
  
  $tmp'v0 := #RegData~typeOf($data);
  ret := ($tmp'v0 == #RegDataType^Variant~NativeObjectSlots());
  return;
}

procedure #RegData~fromNativeObjectSlots($slots: #NativeObjectSlots)
  returns (ret: #RegData)
{
  var $data'v0: #RegData;
  var $tmp'v1: #NativeObjectSlots;
  var out'0: #Bool;
  
  $data'v0 := #RegData~fromNativeObjectSlotsUnchecked($slots);
  call out'0 := #RegData~isNativeObjectSlots($data'v0);
  assume out'0;
  $tmp'v1 := #RegData~toNativeObjectSlotsUnchecked($data'v0);
  assume ($tmp'v1 == $slots);
  ret := $data'v0;
  return;
}

function #RegData~fromNativeObjectSlotsUnchecked($slots: #NativeObjectSlots): #RegData;

procedure #RegData~toNativeObjectSlots($data: #RegData)
  returns (ret: #NativeObjectSlots)
{
  var $slots'v0: #NativeObjectSlots;
  var $tmp'v1: #RegData;
  var out'0: #Bool;
  
  call out'0 := #RegData~isNativeObjectSlots($data);
  assert out'0;
  $slots'v0 := #RegData~toNativeObjectSlotsUnchecked($data);
  $tmp'v1 := #RegData~fromNativeObjectSlotsUnchecked($slots'v0);
  assume ($tmp'v1 == $data);
  ret := $slots'v0;
  return;
}

function #RegData~toNativeObjectSlotsUnchecked($data: #RegData): #NativeObjectSlots;

procedure #RegData~isNativeObjectElements($data: #RegData)
  returns (ret: #Bool)
{
  var $tmp'v0: #RegDataType;
  
  $tmp'v0 := #RegData~typeOf($data);
  ret := ($tmp'v0 == #RegDataType^Variant~NativeObjectElements());
  return;
}

procedure #RegData~fromNativeObjectElements($elements: #NativeObjectElements)
  returns (ret: #RegData)
{
  var $data'v0: #RegData;
  var $tmp'v1: #NativeObjectElements;
  var out'0: #Bool;
  
  $data'v0 := #RegData~fromNativeObjectElementsUnchecked($elements);
  call out'0 := #RegData~isNativeObjectElements($data'v0);
  assume out'0;
  $tmp'v1 := #RegData~toNativeObjectElementsUnchecked($data'v0);
  assume ($tmp'v1 == $elements);
  ret := $data'v0;
  return;
}

function #RegData~fromNativeObjectElementsUnchecked($slots: #NativeObjectElements): #RegData;

procedure #RegData~toNativeObjectElements($data: #RegData)
  returns (ret: #NativeObjectElements)
{
  var $elements'v0: #NativeObjectElements;
  var $tmp'v1: #RegData;
  var out'0: #Bool;
  
  call out'0 := #RegData~isNativeObjectElements($data);
  assert out'0;
  $elements'v0 := #RegData~toNativeObjectElementsUnchecked($data);
  $tmp'v1 := #RegData~fromNativeObjectElementsUnchecked($elements'v0);
  assume ($tmp'v1 == $data);
  ret := $elements'v0;
  return;
}

function #RegData~toNativeObjectElementsUnchecked($data: #RegData): #NativeObjectElements;

procedure #RegData~isTypeTag($data: #RegData)
  returns (ret: #Bool)
{
  var $tmp'v0: #RegDataType;
  
  $tmp'v0 := #RegData~typeOf($data);
  ret := ($tmp'v0 == #RegDataType^Variant~TypeTag());
  return;
}

procedure #RegData~fromValueType($valTy: #ValueType)
  returns (ret: #RegData)
{
  var $data'v0: #RegData;
  var $tmp'v1: #ValueType;
  var out'0: #Bool;
  
  $data'v0 := #RegData~fromValueTypeUnchecked($valTy);
  call out'0 := #RegData~isTypeTag($data'v0);
  assume out'0;
  $tmp'v1 := #RegData~toValueTypeUnchecked($data'v0);
  assume ($tmp'v1 == $valTy);
  ret := $data'v0;
  return;
}

function #RegData~fromValueTypeUnchecked($valTy: #ValueType): #RegData;

procedure #RegData~toValueType($data: #RegData)
  returns (ret: #ValueType)
{
  var $valTy'v0: #ValueType;
  var $tmp'v1: #RegData;
  var out'0: #Bool;
  
  call out'0 := #RegData~isTypeTag($data);
  assert out'0;
  $valTy'v0 := #RegData~toValueTypeUnchecked($data);
  $tmp'v1 := #RegData~fromValueTypeUnchecked($valTy'v0);
  assume ($tmp'v1 == $data);
  ret := $valTy'v0;
  return;
}

function #RegData~toValueTypeUnchecked($data: #RegData): #ValueType;

procedure #RegData~isTaggedProto($data: #RegData)
  returns (ret: #Bool)
{
  var $tmp'v0: #RegDataType;
  
  $tmp'v0 := #RegData~typeOf($data);
  ret := ($tmp'v0 == #RegDataType^Variant~TaggedProto());
  return;
}

procedure #RegData~fromTaggedProto($proto: #TaggedProto)
  returns (ret: #RegData)
{
  var $data'v0: #RegData;
  var $tmp'v1: #TaggedProto;
  var out'0: #Bool;
  
  $data'v0 := #RegData~fromTaggedProtoUnchecked($proto);
  call out'0 := #RegData~isTaggedProto($data'v0);
  assume out'0;
  $tmp'v1 := #RegData~toTaggedProtoUnchecked($data'v0);
  assume ($tmp'v1 == $proto);
  ret := $data'v0;
  return;
}

function #RegData~fromTaggedProtoUnchecked($proto: #TaggedProto): #RegData;

procedure #RegData~toTaggedProto($data: #RegData)
  returns (ret: #TaggedProto)
{
  var $proto'v0: #TaggedProto;
  var $tmp'v1: #RegData;
  var out'0: #Bool;
  
  call out'0 := #RegData~isTaggedProto($data);
  assert out'0;
  $proto'v0 := #RegData~toTaggedProtoUnchecked($data);
  $tmp'v1 := #RegData~fromTaggedProtoUnchecked($proto'v0);
  assume ($tmp'v1 == $data);
  ret := $proto'v0;
  return;
}

function #RegData~toTaggedProtoUnchecked($data: #RegData): #TaggedProto;

procedure #RegData~readData($data: #RegData, $offset: #Int64)
  returns (ret: #RegData)
{
  var $value'v0: #Value;
  var $slots'v1: #NativeObjectSlots;
  var $elements'v2: #NativeObjectElements;
  var out'0: #Bool;
  var out'1: #Value;
  var out'2: #Bool;
  var out'3: #NativeObjectSlots;
  var out'4: #Bool;
  var out'5: #NativeObjectElements;
  
  call out'0 := #RegData~isUnboxedValue($data);
  call out'2 := #RegData~isNativeObjectSlots($data);
  call out'4 := #RegData~isNativeObjectElements($data);
  if (out'0) {
    call out'1 := #RegData~toUnboxedValue($data);
    $value'v0 := out'1;
    call ret := #Value~readData($value'v0, $offset);
    return;
  } else if (out'2) {
    call out'3 := #RegData~toNativeObjectSlots($data);
    $slots'v1 := out'3;
    call ret := #NativeObjectSlots~readData($slots'v1, $offset);
    return;
  } else if (out'4) {
    call out'5 := #RegData~toNativeObjectElements($data);
    $elements'v2 := out'5;
    call ret := #NativeObjectElements~readData($elements'v2, $offset);
    return;
  }
  assert false;
  ret := $data;
  return;
}

function #Value~typeOf($value: #Value): #ValueType;

procedure #Value~isDouble($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #Value~typeOf($value);
  ret := ($tmp'v0 == #ValueType^Variant~Double());
  return;
}

procedure #Value~fromDouble($double: #Double)
  returns (ret: #Value)
{
  var $value'v0: #Value;
  var $tmp'v1: #Double;
  var out'0: #Bool;
  
  $value'v0 := #Value~fromDoubleUnchecked($double);
  call out'0 := #Value~isDouble($value'v0);
  assume out'0;
  $tmp'v1 := #Value~toDoubleUnchecked($value'v0);
  assume ($tmp'v1 == $double);
  ret := $value'v0;
  return;
}

function #Value~fromDoubleUnchecked($value: #Double): #Value;

procedure #Value~toDouble($value: #Value)
  returns (ret: #Double)
{
  var $double'v0: #Double;
  var $tmp'v1: #Value;
  var out'0: #Bool;
  
  call out'0 := #Value~isDouble($value);
  assert out'0;
  $double'v0 := #Value~toDoubleUnchecked($value);
  $tmp'v1 := #Value~fromDoubleUnchecked($double'v0);
  assume ($tmp'v1 == $value);
  ret := $double'v0;
  return;
}

function #Value~toDoubleUnchecked($value: #Value): #Double;

procedure #Value~isInt32($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #Value~typeOf($value);
  ret := ($tmp'v0 == #ValueType^Variant~Int32());
  return;
}

procedure #Value~fromInt32($int32: #Int32)
  returns (ret: #Value)
{
  var $value'v0: #Value;
  var $tmp'v1: #Int32;
  var out'0: #Bool;
  
  $value'v0 := #Value~fromInt32Unchecked($int32);
  call out'0 := #Value~isInt32($value'v0);
  assume out'0;
  $tmp'v1 := #Value~toInt32Unchecked($value'v0);
  assume ($tmp'v1 == $int32);
  ret := $value'v0;
  return;
}

function #Value~fromInt32Unchecked($value: #Int32): #Value;

procedure #Value~toInt32($value: #Value)
  returns (ret: #Int32)
{
  var $int32'v0: #Int32;
  var $tmp'v1: #Value;
  var out'0: #Bool;
  
  call out'0 := #Value~isInt32($value);
  assert out'0;
  $int32'v0 := #Value~toInt32Unchecked($value);
  $tmp'v1 := #Value~fromInt32Unchecked($int32'v0);
  assume ($tmp'v1 == $value);
  ret := $int32'v0;
  return;
}

function #Value~toInt32Unchecked($value: #Value): #Int32;

procedure #Value~isNumber($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #Bool;
  var $tmp'v1: #Bool;
  var $tmp'v2: #Bool;
  var out'0: #Bool;
  var out'1: #Bool;
  
  $tmp'v2 := true;
  call out'0 := #Value~isDouble($value);
  $tmp'v0 := out'0;
  if (!$tmp'v0) {
    call out'1 := #Value~isInt32($value);
    $tmp'v1 := out'1;
    $tmp'v2 := $tmp'v1;
  }
  ret := $tmp'v2;
  return;
}

procedure #Value~toNumber($value: #Value)
  returns (ret: #Double)
{
  var out'0: #Bool;
  var out'1: #Bool;
  var out'2: #Int32;
  
  call out'0 := #Value~isNumber($value);
  assert out'0;
  call out'1 := #Value~isInt32($value);
  if (out'1) {
    call out'2 := #Value~toInt32($value);
    ret := #Int32^to#Double(out'2);
    return;
  }
  call ret := #Value~toDouble($value);
  return;
}

procedure #Value~isBool($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #Value~typeOf($value);
  ret := ($tmp'v0 == #ValueType^Variant~Bool());
  return;
}

procedure #Value~fromBool($bool: #Bool)
  returns (ret: #Value)
{
  var $value'v0: #Value;
  var $tmp'v1: #Bool;
  var out'0: #Bool;
  
  $value'v0 := #Value~fromBoolUnchecked($bool);
  call out'0 := #Value~isBool($value'v0);
  assume out'0;
  $tmp'v1 := #Value~toBoolUnchecked($value'v0);
  assume ($tmp'v1 == $bool);
  ret := $value'v0;
  return;
}

function #Value~fromBoolUnchecked($value: #Bool): #Value;

procedure #Value~toBool($value: #Value)
  returns (ret: #Bool)
{
  var $bool'v0: #Bool;
  var $tmp'v1: #Value;
  var out'0: #Bool;
  
  call out'0 := #Value~isBool($value);
  assert out'0;
  $bool'v0 := #Value~toBoolUnchecked($value);
  $tmp'v1 := #Value~fromBoolUnchecked($bool'v0);
  assume ($tmp'v1 == $value);
  ret := $bool'v0;
  return;
}

function #Value~toBoolUnchecked($value: #Value): #Bool;

procedure #Value~isNull($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #Value~typeOf($value);
  ret := ($tmp'v0 == #ValueType^Variant~Null());
  return;
}

procedure #Value~getUndefined()
  returns (ret: #Value)
{
  var $value'v0: #Value;
  var out'0: #Bool;
  
  $value'v0 := #Value~getUndefinedUnchecked();
  call out'0 := #Value~isUndefined($value'v0);
  assume out'0;
  ret := $value'v0;
  return;
}

function #Value~getUndefinedUnchecked(): #Value;

procedure #Value~isUndefined($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #Value~typeOf($value);
  ret := ($tmp'v0 == #ValueType^Variant~Undefined());
  return;
}

procedure #Value~isObject($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #Value~typeOf($value);
  ret := ($tmp'v0 == #ValueType^Variant~Object());
  return;
}

procedure #Value~fromObject($object: #Object)
  returns (ret: #Value)
{
  var $value'v0: #Value;
  var $tmp'v1: #Object;
  var out'0: #Bool;
  
  $value'v0 := #Value~fromObjectUnchecked($object);
  call out'0 := #Value~isObject($value'v0);
  assume out'0;
  $tmp'v1 := #Value~toObjectUnchecked($value'v0);
  assume ($tmp'v1 == $object);
  ret := $value'v0;
  return;
}

function #Value~fromObjectUnchecked($value: #Object): #Value;

procedure #Value~toObject($value: #Value)
  returns (ret: #Object)
{
  var $object'v0: #Object;
  var $tmp'v1: #Value;
  var out'0: #Bool;
  
  call out'0 := #Value~isObject($value);
  assert out'0;
  $object'v0 := #Value~toObjectUnchecked($value);
  $tmp'v1 := #Value~fromObjectUnchecked($object'v0);
  assume ($tmp'v1 == $value);
  ret := $object'v0;
  return;
}

function #Value~toObjectUnchecked($value: #Value): #Object;

procedure #Value~isMagic($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #Value~typeOf($value);
  ret := ($tmp'v0 == #ValueType^Variant~Magic());
  return;
}

procedure #Value~isString($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #Value~typeOf($value);
  ret := ($tmp'v0 == #ValueType^Variant~String());
  return;
}

procedure #Value~fromString($string: #String)
  returns (ret: #Value)
{
  var $value'v0: #Value;
  var $tmp'v1: #String;
  var out'0: #Bool;
  
  $value'v0 := #Value~fromStringUnchecked($string);
  call out'0 := #Value~isString($value'v0);
  assume out'0;
  $tmp'v1 := #Value~toStringUnchecked($value'v0);
  assume ($tmp'v1 == $string);
  ret := $value'v0;
  return;
}

function #Value~fromStringUnchecked($string: #String): #Value;

procedure #Value~toString($value: #Value)
  returns (ret: #String)
{
  var $string'v0: #String;
  var $tmp'v1: #Value;
  var out'0: #Bool;
  
  call out'0 := #Value~isString($value);
  assert out'0;
  $string'v0 := #Value~toStringUnchecked($value);
  $tmp'v1 := #Value~fromStringUnchecked($string'v0);
  assume ($tmp'v1 == $value);
  ret := $string'v0;
  return;
}

function #Value~toStringUnchecked($value: #Value): #String;

procedure #Value~isSymbol($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #Value~typeOf($value);
  ret := ($tmp'v0 == #ValueType^Variant~Symbol());
  return;
}

procedure #Value~fromSymbol($symbol: #Symbol)
  returns (ret: #Value)
{
  var $value'v0: #Value;
  var $tmp'v1: #Symbol;
  var out'0: #Bool;
  
  $value'v0 := #Value~fromSymbolUnchecked($symbol);
  call out'0 := #Value~isSymbol($value'v0);
  assume out'0;
  $tmp'v1 := #Value~toSymbolUnchecked($value'v0);
  assume ($tmp'v1 == $symbol);
  ret := $value'v0;
  return;
}

function #Value~fromSymbolUnchecked($symbol: #Symbol): #Value;

procedure #Value~toSymbol($value: #Value)
  returns (ret: #Symbol)
{
  var $symbol'v0: #Symbol;
  var $tmp'v1: #Value;
  var out'0: #Bool;
  
  call out'0 := #Value~isSymbol($value);
  assert out'0;
  $symbol'v0 := #Value~toSymbolUnchecked($value);
  $tmp'v1 := #Value~fromSymbolUnchecked($symbol'v0);
  assume ($tmp'v1 == $value);
  ret := $symbol'v0;
  return;
}

function #Value~toSymbolUnchecked($value: #Value): #Symbol;

procedure #Value~isBigInt($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #Value~typeOf($value);
  ret := ($tmp'v0 == #ValueType^Variant~BigInt());
  return;
}

procedure #Value~fromBigInt($bigInt: #BigInt)
  returns (ret: #Value)
{
  var $value'v0: #Value;
  var $tmp'v1: #BigInt;
  var out'0: #Bool;
  
  $value'v0 := #Value~fromBigIntUnchecked($bigInt);
  call out'0 := #Value~isBigInt($value'v0);
  assume out'0;
  $tmp'v1 := #Value~toBigIntUnchecked($value'v0);
  assume ($tmp'v1 == $bigInt);
  ret := $value'v0;
  return;
}

function #Value~fromBigIntUnchecked($bigInt: #BigInt): #Value;

procedure #Value~toBigInt($value: #Value)
  returns (ret: #BigInt)
{
  var $bigInt'v0: #BigInt;
  var $tmp'v1: #Value;
  var out'0: #Bool;
  
  call out'0 := #Value~isBigInt($value);
  assert out'0;
  $bigInt'v0 := #Value~toBigIntUnchecked($value);
  $tmp'v1 := #Value~fromBigIntUnchecked($bigInt'v0);
  assume ($tmp'v1 == $value);
  ret := $bigInt'v0;
  return;
}

function #Value~toBigIntUnchecked($value: #Value): #BigInt;

procedure #Value~readData($value: #Value, $offset: #Int64)
  returns (ret: #RegData)
{
  var $object'v0: #Object;
  var out'0: #Bool;
  var out'1: #Object;
  
  call out'0 := #Value~isObject($value);
  if (out'0) {
    call out'1 := #Value~toObject($value);
    $object'v0 := out'1;
    call ret := #Object~readData($object'v0, $offset);
    return;
  }
  assert false;
  call ret := #RegData~fromValue($value);
  return;
}

procedure #Object~shapeOf($object: #Object)
  returns (ret: #Shape)
{
  var $tmp'v0: #Heap;
  
  $tmp'v0 := #heap;
  ret := #Object~shapeOfUnchecked($tmp'v0, $object);
  return;
}

function #Object~shapeOfUnchecked($heap: #Heap, $object: #Object): #Shape;

procedure #Object~toNativeObject($object: #Object)
  returns (ret: #NativeObject)
{
  var $shape'v0: #Shape;
  var $class'v1: #Class;
  var out'0: #Shape;
  var out'1: #Class;
  
  call out'0 := #Object~shapeOf($object);
  $shape'v0 := out'0;
  call out'1 := #Shape~classOf($shape'v0);
  $class'v1 := out'1;
  assert #Class~isNativeObject($class'v1);
  ret := #Object^to#NativeObject($object);
  return;
}

procedure #Object~toArgumentsObject($object: #Object)
  returns (ret: #ArgumentsObject)
{
  var $shape'v0: #Shape;
  var $class'v1: #Class;
  var out'0: #Shape;
  var out'1: #Class;
  var out'2: #Bool;
  
  call out'0 := #Object~shapeOf($object);
  $shape'v0 := out'0;
  call out'1 := #Shape~classOf($shape'v0);
  $class'v1 := out'1;
  call out'2 := #Class~isArgumentsObject($class'v1);
  assert out'2;
  ret := #NativeObject^to#ArgumentsObject(#Object^to#NativeObject($object));
  return;
}

procedure #Object~getFixedSlot($object: #Object, $slot: #UInt32)
  returns (ret: #Value)
{
  var $nativeObject'v0: #NativeObject;
  var out'0: #NativeObject;
  
  call out'0 := #Object~toNativeObject($object);
  $nativeObject'v0 := out'0;
  call ret := #NativeObject~getFixedSlot($nativeObject'v0, $slot);
  return;
}

procedure #Object~readData($object: #Object, $offset: #Int64)
  returns (ret: #RegData)
{
  var $shape'v0: #Shape;
  var $class'v1: #Class;
  var $nativeObject'v2: #NativeObject;
  var $tmp'v3: #Value;
  var out'0: #Shape;
  var out'1: #Class;
  var out'2: #Value;
  
  call out'0 := #Object~shapeOf($object);
  $shape'v0 := out'0;
  call out'1 := #Shape~classOf($shape'v0);
  $class'v1 := out'1;
  if (#Class~isNativeObject($class'v1)) {
    $nativeObject'v2 := #Object^to#NativeObject($object);
    call ret := #NativeObject~readData($nativeObject'v2, $offset);
    return;
  }
  assert false;
  call out'2 := #Value~fromObject($object);
  $tmp'v3 := out'2;
  call ret := #RegData~fromValue($tmp'v3);
  return;
}

procedure #NativeObject~getFixedSlot($nativeObject: #NativeObject, $slot: #UInt32)
  returns (ret: #Value)
{
  var $shape'v0: #Shape;
  var $tmp'v1: #Heap;
  var out'0: #Shape;
  var out'1: #Bool;
  
  call out'0 := #Object~shapeOf(#NativeObject^to#Object($nativeObject));
  $shape'v0 := out'0;
  call out'1 := #Shape~hasFixedSlot($shape'v0, $slot);
  assert out'1;
  $tmp'v1 := #heap;
  ret := #NativeObject~getFixedSlotUnchecked($tmp'v1, $nativeObject, $slot);
  return;
}

function #NativeObject~getFixedSlotUnchecked($heap: #Heap, $nativeObject: #NativeObject, $slot: #UInt32): #Value;

procedure #NativeObject~getSlots($nativeObject: #NativeObject)
  returns (ret: #NativeObjectSlots)
{
  var $slots'v0: #NativeObjectSlots;
  var $shape'v1: #Shape;
  var $tmp'v2: #Heap;
  var $tmp'v3: #UInt32;
  var $tmp'v4: #UInt32;
  var $tmp'v5: #UInt32;
  var $tmp'v6: #UInt32;
  var out'0: #Shape;
  
  $tmp'v2 := #heap;
  $slots'v0 := #NativeObject~getSlotsUnchecked($tmp'v2, $nativeObject);
  call out'0 := #Object~shapeOf(#NativeObject^to#Object($nativeObject));
  $shape'v1 := out'0;
  $tmp'v3 := #NativeObjectSlots~length($slots'v0);
  $tmp'v4 := #Shape~slotSpan($shape'v1);
  $tmp'v5 := #Shape~numFixedSlots($shape'v1);
  $tmp'v6 := #UInt32^sub($tmp'v4, $tmp'v5);
  assume ($tmp'v3 == $tmp'v6);
  ret := $slots'v0;
  return;
}

function #NativeObject~getSlotsUnchecked($heap: #Heap, $nativeObject: #NativeObject): #NativeObjectSlots;

procedure #NativeObject~getElementsHeader($nativeObject: #NativeObject)
  returns (ret: #NativeObjectElements)
{
  var $tmp'v0: #Heap;
  
  $tmp'v0 := #heap;
  ret := #NativeObject~getElementsHeaderUnchecked($tmp'v0, $nativeObject);
  return;
}

function #NativeObject~getElementsHeaderUnchecked($heap: #Heap, $nativeObject: #NativeObject): #NativeObjectElements;

procedure #NativeObject~readData($nativeObject: #NativeObject, $offset: #Int64)
  returns (ret: #RegData)
{
  var $slots'v0: #NativeObjectSlots;
  var $elements'v1: #NativeObjectElements;
  var $slotOffset'v2: #UInt64;
  var $slot'v3: #UInt32;
  var $value'v4: #Value;
  var $tmp'v5: #Value;
  var out'0: #NativeObjectSlots;
  var out'1: #NativeObjectElements;
  var out'2: #Value;
  var out'3: #Value;
  
  if ($offset == #UInt64^to#Int64(#NativeObject~offsetOfSlots())) {
    call out'0 := #NativeObject~getSlots($nativeObject);
    $slots'v0 := out'0;
    call ret := #RegData~fromNativeObjectSlots($slots'v0);
    return;
  } else if ($offset == #UInt64^to#Int64(#NativeObject~offsetOfElements())) {
    call out'1 := #NativeObject~getElementsHeader($nativeObject);
    $elements'v1 := out'1;
    call ret := #RegData~fromNativeObjectElements($elements'v1);
    return;
  } else if (#Int64^gte($offset, #UInt64^to#Int64(#NativeObject~sizeOf()))) {
    $slotOffset'v2 := #UInt64^sub(#Int64^to#UInt64($offset), #NativeObject~sizeOf());
    assert (#UInt64^mod($slotOffset'v2, #Value~sizeOf()) == 0bv64);
    assert #UInt64^lt(#UInt64^div($slotOffset'v2, #Value~sizeOf()), #NativeObject~maxFixedSlots());
    $slot'v3 := #UInt64^to#UInt32(#UInt64^div($slotOffset'v2, #Value~sizeOf()));
    call out'2 := #NativeObject~getFixedSlot($nativeObject, $slot'v3);
    $value'v4 := out'2;
    call ret := #RegData~fromValue($value'v4);
    return;
  }
  assert false;
  call out'3 := #Value~fromObject(#NativeObject^to#Object($nativeObject));
  $tmp'v5 := out'3;
  call ret := #RegData~fromValue($tmp'v5);
  return;
}

function #NativeObjectSlots~length($slots: #NativeObjectSlots): #UInt32;

procedure #NativeObjectSlots~getDynamicSlot($slots: #NativeObjectSlots, $slot: #UInt32)
  returns (ret: #Value)
{
  var $tmp'v0: #UInt32;
  var $tmp'v1: #Heap;
  
  $tmp'v0 := #NativeObjectSlots~length($slots);
  assert #UInt32^lt($slot, $tmp'v0);
  $tmp'v1 := #heap;
  ret := #NativeObjectSlots~getDynamicSlotUnchecked($tmp'v1, $slots, $slot);
  return;
}

function #NativeObjectSlots~getDynamicSlotUnchecked($heap: #Heap, $slots: #NativeObjectSlots, $slot: #UInt32): #Value;

procedure #NativeObjectSlots~readData($slots: #NativeObjectSlots, $offset: #Int64)
  returns (ret: #RegData)
{
  var $slot'v0: #UInt32;
  var $value'v1: #Value;
  var out'0: #Value;
  
  assert (#Int64^mod($offset, #UInt64^to#Int64(#Value~sizeOf())) == 0bv64);
  $slot'v0 := #Int64^to#UInt32(#Int64^div($offset, #UInt64^to#Int64(#Value~sizeOf())));
  call out'0 := #NativeObjectSlots~getDynamicSlot($slots, $slot'v0);
  $value'v1 := out'0;
  call ret := #RegData~fromValue($value'v1);
  return;
}

procedure #NativeObjectElements~getLength($elements: #NativeObjectElements)
  returns (ret: #UInt32)
{
  var $tmp'v0: #Heap;
  
  $tmp'v0 := #heap;
  ret := #NativeObjectElements~getLengthUnchecked($tmp'v0, $elements);
  return;
}

function #NativeObjectElements~getLengthUnchecked($heap: #Heap, $elements: #NativeObjectElements): #UInt32;

procedure #NativeObjectElements~getInitializedLength($elements: #NativeObjectElements)
  returns (ret: #UInt32)
{
  var $tmp'v0: #Heap;
  
  $tmp'v0 := #heap;
  ret := #NativeObjectElements~getInitializedLengthUnchecked($tmp'v0, $elements);
  return;
}

function #NativeObjectElements~getInitializedLengthUnchecked($heap: #Heap, $elements: #NativeObjectElements): #UInt32;

procedure #NativeObjectElements~readData($elements: #NativeObjectElements, $offset: #Int64)
  returns (ret: #RegData)
{
  var $length'v0: #UInt32;
  var $value'v1: #Value;
  var $initializedLength'v2: #UInt32;
  var $value'v3: #Value;
  var out'0: #UInt32;
  var out'1: #Value;
  var out'2: #UInt32;
  var out'3: #Value;
  
  if ($offset == #Int32^to#Int64(#NativeObjectElements~offsetOfLength())) {
    call out'0 := #NativeObjectElements~getLength($elements);
    $length'v0 := out'0;
    call out'1 := #Value~fromInt32(#UInt32^to#Int32($length'v0));
    $value'v1 := out'1;
    call ret := #RegData~fromValue($value'v1);
    return;
  } else if ($offset == #Int32^to#Int64(#NativeObjectElements~offsetOfInitializedLength())) {
    call out'2 := #NativeObjectElements~getInitializedLength($elements);
    $initializedLength'v2 := out'2;
    call out'3 := #Value~fromInt32(#UInt32^to#Int32($initializedLength'v2));
    $value'v3 := out'3;
    call ret := #RegData~fromValue($value'v3);
    return;
  }
  assert false;
  call ret := #RegData~fromNativeObjectElements($elements);
  return;
}

procedure #ArrayObject~class()
  returns (ret: #Class)
{
  assume #Class~isNativeObject(#ArrayObject~rawClass);
  ret := #ArrayObject~rawClass;
  return;
}

procedure #PlainObject~class()
  returns (ret: #Class)
{
  assume #Class~isNativeObject(#PlainObject~rawClass);
  ret := #PlainObject~rawClass;
  return;
}

procedure #ArrayBufferObject~class()
  returns (ret: #Class)
{
  assume #Class~isNativeObject(#ArrayBufferObject~rawClass);
  ret := #ArrayBufferObject~rawClass;
  return;
}

procedure #SharedArrayBufferObject~class()
  returns (ret: #Class)
{
  assume #Class~isNativeObject(#SharedArrayBufferObject~rawClass);
  ret := #SharedArrayBufferObject~rawClass;
  return;
}

procedure #DataViewObject~class()
  returns (ret: #Class)
{
  assume #Class~isNativeObject(#DataViewObject~rawClass);
  ret := #DataViewObject~rawClass;
  return;
}

function #ArgumentsObject~getInitialLength($obj: #ArgumentsObject): #Int32;

function #ArgumentsObject~hasOverriddenLength($obj: #ArgumentsObject): #Bool;

procedure #UnmappedArgumentsObject~class()
  returns (ret: #Class)
{
  assume #Class~isNativeObject(#UnmappedArgumentsObject~rawClass);
  ret := #UnmappedArgumentsObject~rawClass;
  return;
}

procedure #MappedArgumentsObject~class()
  returns (ret: #Class)
{
  assume #Class~isNativeObject(#MappedArgumentsObject~rawClass);
  ret := #MappedArgumentsObject~rawClass;
  return;
}

procedure #SetObject~class()
  returns (ret: #Class)
{
  assume #Class~isNativeObject(#SetObject~rawClass);
  ret := #SetObject~rawClass;
  return;
}

procedure #MapObject~class()
  returns (ret: #Class)
{
  assume #Class~isNativeObject(#MapObject~rawClass);
  ret := #MapObject~rawClass;
  return;
}

function #BaseShape~fromAddr($addr: #UInt64): #BaseShape;

function #BaseShape~classOf($baseShape: #BaseShape): #Class;

procedure #BaseShape~protoOf($baseShape: #BaseShape)
  returns (ret: #TaggedProto)
{
  var $tmp'v0: #Heap;
  
  $tmp'v0 := #heap;
  ret := #BaseShape~protoOfUnchecked($tmp'v0, $baseShape);
  return;
}

function #BaseShape~protoOfUnchecked($heap: #Heap, $baseShape: #BaseShape): #TaggedProto;

function #Shape~fromAddr($addr: #UInt64): #Shape;

function #Shape~baseShapeOf($shape: #Shape): #BaseShape;

procedure #Shape~classOf($shape: #Shape)
  returns (ret: #Class)
{
  var $tmp'v0: #BaseShape;
  
  $tmp'v0 := #Shape~baseShapeOf($shape);
  ret := #BaseShape~classOf($tmp'v0);
  return;
}

function #Shape~numFixedSlots($shape: #Shape): #UInt32;

function #Shape~slotSpan($shape: #Shape): #UInt32;

procedure #Shape~hasFixedSlot($shape: #Shape, $slot: #UInt32)
  returns (ret: #Bool)
{
  var $tmp'v0: #UInt32;
  
  $tmp'v0 := #Shape~numFixedSlots($shape);
  ret := #UInt32^lt($slot, $tmp'v0);
  return;
}

function #Class~fromAddr($addr: #UInt64): #Class;

function #Class~windowProxyClass(): #Class;

function #Class~isNativeObject($class: #Class): #Bool;

function #Class~isProxyObject($class: #Class): #Bool;

procedure #Class~isArgumentsObject($class: #Class)
  returns (ret: #Bool)
{
  var $tmp'v0: #Class;
  var $tmp'v1: #Bool;
  var $tmp'v2: #Class;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Class;
  var out'1: #Class;
  
  $tmp'v4 := true;
  call out'0 := #MappedArgumentsObject~class();
  $tmp'v0 := out'0;
  $tmp'v1 := ($class == $tmp'v0);
  if (!$tmp'v1) {
    call out'1 := #UnmappedArgumentsObject~class();
    $tmp'v2 := out'1;
    $tmp'v3 := ($class == $tmp'v2);
    $tmp'v4 := $tmp'v3;
  }
  ret := $tmp'v4;
  return;
}

procedure #Class~functionClass()
  returns (ret: #Class)
{
  assume #Class~isNativeObject(#Class~rawFunctionClass);
  ret := #Class~rawFunctionClass;
  return;
}

procedure #Class~extendedFunctionClass()
  returns (ret: #Class)
{
  assume #Class~isNativeObject(#Class~rawExtendedFunctionClass);
  ret := #Class~rawExtendedFunctionClass;
  return;
}

function #TaggedProto~fromAddr($addr: #UInt64): #TaggedProto;

function #TaggedProto~tag($proto: #TaggedProto): #ProtoTag;

procedure #TaggedProto~isNull($proto: #TaggedProto)
  returns (ret: #Bool)
{
  var $tmp'v0: #ProtoTag;
  
  $tmp'v0 := #TaggedProto~tag($proto);
  ret := ($tmp'v0 == #ProtoTag^Variant~Null());
  return;
}

procedure #TaggedProto~isLazy($proto: #TaggedProto)
  returns (ret: #Bool)
{
  var $tmp'v0: #ProtoTag;
  
  $tmp'v0 := #TaggedProto~tag($proto);
  ret := ($tmp'v0 == #ProtoTag^Variant~Lazy());
  return;
}

procedure #TaggedProto~isObject($proto: #TaggedProto)
  returns (ret: #Bool)
{
  var $tmp'v0: #ProtoTag;
  
  $tmp'v0 := #TaggedProto~tag($proto);
  ret := ($tmp'v0 == #ProtoTag^Variant~Object());
  return;
}

procedure #TaggedProto~toObject($proto: #TaggedProto)
  returns (ret: #Object)
{
  var out'0: #Bool;
  
  call out'0 := #TaggedProto~isObject($proto);
  assert out'0;
  ret := #TaggedProto~toObjectUnchecked($proto);
  return;
}

function #TaggedProto~toObjectUnchecked($proto: #TaggedProto): #Object;

procedure #Address~new($base: #Reg, $offset: #Int32)
  returns (ret: #Address)
{
  var $address'v0: #Address;
  
  $address'v0 := #Address~newUnchecked($base, $offset);
  assume (#Address^field~base($address'v0) == $base);
  assume (#Address^field~offset($address'v0) == $offset);
  ret := $address'v0;
  return;
}

function #Address~newUnchecked($base: #Reg, $offset: #Int32): #Address;

procedure #BaseIndex~new($base: #Reg, $index: #Reg, $scale: #Scale, $offset: #UInt32)
  returns (ret: #BaseIndex)
{
  var $baseIndex'v0: #BaseIndex;
  
  $baseIndex'v0 := #BaseIndex~newUnchecked($base, $index, $scale, $offset);
  assume (#BaseIndex^field~base($baseIndex'v0) == $base);
  assume (#BaseIndex^field~index($baseIndex'v0) == $index);
  assume (#BaseIndex^field~scale($baseIndex'v0) == $scale);
  assume (#BaseIndex^field~offset($baseIndex'v0) == $offset);
  ret := $baseIndex'v0;
  return;
}

function #BaseIndex~newUnchecked($base: #Reg, $index: #Reg, $scale: #Scale, $offset: #UInt32): #BaseIndex;

procedure #BaseValueIndex~new($base: #Reg, $index: #Reg, $offset: #UInt32)
  returns (ret: #BaseValueIndex)
{
  var $inner'v0: #BaseIndex;
  var $baseValueIndex'v1: #BaseValueIndex;
  var out'0: #BaseIndex;
  
  call out'0 := #BaseIndex~new($base, $index, #ValueScale(), $offset);
  $inner'v0 := out'0;
  $baseValueIndex'v1 := #BaseValueIndex~newUnchecked($inner'v0);
  assume (#BaseValueIndex^field~inner($baseValueIndex'v1) == $inner'v0);
  ret := $baseValueIndex'v1;
  return;
}

function #BaseValueIndex~newUnchecked($inner: #BaseIndex): #BaseValueIndex;

procedure #BaseObjectElementIndex~new($base: #Reg, $index: #Reg, $offset: #UInt32)
  returns (ret: #BaseObjectElementIndex)
{
  var $inner'v0: #BaseValueIndex;
  var $baseObjectElementIndex'v1: #BaseObjectElementIndex;
  var out'0: #BaseValueIndex;
  
  call out'0 := #BaseValueIndex~new($base, $index, $offset);
  $inner'v0 := out'0;
  $baseObjectElementIndex'v1 := #BaseObjectElementIndex~newUnchecked($inner'v0);
  assume (#BaseObjectElementIndex^field~inner($baseObjectElementIndex'v1) == $inner'v0);
  ret := $baseObjectElementIndex'v1;
  return;
}

function #BaseObjectElementIndex~newUnchecked($inner: #BaseValueIndex): #BaseObjectElementIndex;

procedure #BaseObjectSlotIndex~new($base: #Reg, $index: #Reg)
  returns (ret: #BaseObjectSlotIndex)
{
  var $inner'v0: #BaseValueIndex;
  var $baseObjectSlotIndex'v1: #BaseObjectSlotIndex;
  var out'0: #BaseValueIndex;
  
  call out'0 := #BaseValueIndex~new($base, $index, 0bv32);
  $inner'v0 := out'0;
  $baseObjectSlotIndex'v1 := #BaseObjectSlotIndex~newUnchecked($inner'v0);
  assume (#BaseObjectSlotIndex^field~inner($baseObjectSlotIndex'v1) == $inner'v0);
  ret := $baseObjectSlotIndex'v1;
  return;
}

function #BaseObjectSlotIndex~newUnchecked($inner: #BaseValueIndex): #BaseObjectSlotIndex;

function #Operand~fromBaseIndex($baseIndex: #BaseIndex): #Operand;

function #Operand~compareToReg($cond: #Condition, $operand: #Operand, $reg: #Reg): #Bool;

procedure #MASM~getUnboxedValue($reg: #Reg)
  returns (ret: #Value)
{
  var $tmp'v0: #RegData;
  var out'0: #RegData;
  
  call out'0 := #MASM~getData($reg);
  $tmp'v0 := out'0;
  call ret := #RegData~toUnboxedValue($tmp'v0);
  return;
}

procedure #MASM~setUnboxedValue($reg: #Reg, $value: #Value)
{
  var $tmp'v0: #RegData;
  var out'0: #RegData;
  
  call out'0 := #RegData~fromUnboxedValue($value);
  $tmp'v0 := out'0;
  call #MASM~setData($reg, $tmp'v0);
  return;
}

procedure #MASM~getInt32($reg: #Reg)
  returns (ret: #Int32)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #MASM~getUnboxedValue($reg);
  $tmp'v0 := out'0;
  call ret := #Value~toInt32($tmp'v0);
  return;
}

procedure #MASM~setInt32($reg: #Reg, $int32: #Int32)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #Value~fromInt32($int32);
  $tmp'v0 := out'0;
  call #MASM~setUnboxedValue($reg, $tmp'v0);
  return;
}

procedure #MASM~getBool($reg: #Reg)
  returns (ret: #Bool)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #MASM~getUnboxedValue($reg);
  $tmp'v0 := out'0;
  call ret := #Value~toBool($tmp'v0);
  return;
}

procedure #MASM~setBool($reg: #Reg, $bool: #Bool)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #Value~fromBool($bool);
  $tmp'v0 := out'0;
  call #MASM~setUnboxedValue($reg, $tmp'v0);
  return;
}

procedure #MASM~getObject($reg: #Reg)
  returns (ret: #Object)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #MASM~getUnboxedValue($reg);
  $tmp'v0 := out'0;
  call ret := #Value~toObject($tmp'v0);
  return;
}

procedure #MASM~setObject($reg: #Reg, $object: #Object)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #Value~fromObject($object);
  $tmp'v0 := out'0;
  call #MASM~setUnboxedValue($reg, $tmp'v0);
  return;
}

procedure #MASM~getString($reg: #Reg)
  returns (ret: #String)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #MASM~getUnboxedValue($reg);
  $tmp'v0 := out'0;
  call ret := #Value~toString($tmp'v0);
  return;
}

procedure #MASM~setString($reg: #Reg, $string: #String)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #Value~fromString($string);
  $tmp'v0 := out'0;
  call #MASM~setUnboxedValue($reg, $tmp'v0);
  return;
}

procedure #MASM~getSymbol($reg: #Reg)
  returns (ret: #Symbol)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #MASM~getUnboxedValue($reg);
  $tmp'v0 := out'0;
  call ret := #Value~toSymbol($tmp'v0);
  return;
}

procedure #MASM~setSymbol($reg: #Reg, $symbol: #Symbol)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #Value~fromSymbol($symbol);
  $tmp'v0 := out'0;
  call #MASM~setUnboxedValue($reg, $tmp'v0);
  return;
}

procedure #MASM~getBigInt($reg: #Reg)
  returns (ret: #BigInt)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #MASM~getUnboxedValue($reg);
  $tmp'v0 := out'0;
  call ret := #Value~toBigInt($tmp'v0);
  return;
}

procedure #MASM~setBigInt($reg: #Reg, $bigInt: #BigInt)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #Value~fromBigInt($bigInt);
  $tmp'v0 := out'0;
  call #MASM~setUnboxedValue($reg, $tmp'v0);
  return;
}

function #OperandLocation~kind($loc: #OperandLocation): #OperandLocationKind;

procedure #OperandLocation~newUninitialized()
  returns (ret: #OperandLocation)
{
  var $loc'v0: #OperandLocation;
  var $tmp'v1: #OperandLocationKind;
  
  $loc'v0 := #OperandLocation~newUninitializedUnchecked();
  $tmp'v1 := #OperandLocation~kind($loc'v0);
  assume ($tmp'v1 == #OperandLocationKind^Variant~Uninitialized());
  ret := $loc'v0;
  return;
}

function #OperandLocation~newUninitializedUnchecked(): #OperandLocation;

procedure #OperandLocation~setValueReg($valueReg: #ValueReg)
  returns ($loc: #OperandLocation)
{
  var $tmp'v0: #OperandLocationKind;
  var $tmp'v1: #ValueReg;
  
  call $loc := #OperandLocation~setValueRegUnchecked($valueReg);
  $tmp'v0 := #OperandLocation~kind($loc);
  assume ($tmp'v0 == #OperandLocationKind^Variant~ValueReg());
  $tmp'v1 := #OperandLocation~getValueRegUnchecked($loc);
  assume ($tmp'v1 == $valueReg);
  return;
}

function #OperandLocation~setValueRegUnchecked^$loc($valueReg: #ValueReg): #OperandLocation;

procedure {:inline 1} #OperandLocation~setValueRegUnchecked($valueReg: #ValueReg)
  returns ($loc: #OperandLocation)
{
  $loc := #OperandLocation~setValueRegUnchecked^$loc($valueReg);
}

procedure #OperandLocation~getValueReg($loc: #OperandLocation)
  returns (ret: #ValueReg)
{
  var $valueReg'v0: #ValueReg;
  var $tmp'v1: #OperandLocationKind;
  
  $tmp'v1 := #OperandLocation~kind($loc);
  assert ($tmp'v1 == #OperandLocationKind^Variant~ValueReg());
  $valueReg'v0 := #OperandLocation~getValueRegUnchecked($loc);
  ret := $valueReg'v0;
  return;
}

function #OperandLocation~getValueRegUnchecked($loc: #OperandLocation): #ValueReg;

procedure #OperandLocation~setPayloadReg($reg: #Reg, $type: #ValueType)
  returns ($loc: #OperandLocation)
{
  var $tmp'v0: #OperandLocationKind;
  var $tmp'v1: #Reg;
  var $tmp'v2: #ValueType;
  
  call $loc := #OperandLocation~setPayloadRegUnchecked($reg, $type);
  $tmp'v0 := #OperandLocation~kind($loc);
  assume ($tmp'v0 == #OperandLocationKind^Variant~PayloadReg());
  $tmp'v1 := #OperandLocation~getPayloadRegUnchecked($loc);
  assume ($tmp'v1 == $reg);
  $tmp'v2 := #OperandLocation~getPayloadTypeUnchecked($loc);
  assume ($tmp'v2 == $type);
  return;
}

function #OperandLocation~setPayloadRegUnchecked^$loc($reg: #Reg, $type: #ValueType): #OperandLocation;

procedure {:inline 1} #OperandLocation~setPayloadRegUnchecked($reg: #Reg, $type: #ValueType)
  returns ($loc: #OperandLocation)
{
  $loc := #OperandLocation~setPayloadRegUnchecked^$loc($reg, $type);
}

procedure #OperandLocation~getPayloadReg($loc: #OperandLocation)
  returns (ret: #Reg)
{
  var $reg'v0: #Reg;
  var $tmp'v1: #OperandLocationKind;
  
  $tmp'v1 := #OperandLocation~kind($loc);
  assert ($tmp'v1 == #OperandLocationKind^Variant~PayloadReg());
  $reg'v0 := #OperandLocation~getPayloadRegUnchecked($loc);
  ret := $reg'v0;
  return;
}

function #OperandLocation~getPayloadRegUnchecked($loc: #OperandLocation): #Reg;

procedure #OperandLocation~getPayloadType($loc: #OperandLocation)
  returns (ret: #ValueType)
{
  var $type'v0: #ValueType;
  var $tmp'v1: #OperandLocationKind;
  
  $tmp'v1 := #OperandLocation~kind($loc);
  assert ($tmp'v1 == #OperandLocationKind^Variant~PayloadReg());
  $type'v0 := #OperandLocation~getPayloadTypeUnchecked($loc);
  ret := $type'v0;
  return;
}

function #OperandLocation~getPayloadTypeUnchecked($loc: #OperandLocation): #ValueType;

function #OperandId~id($operandId: #OperandId): #UInt16;

procedure #OperandId~fromId($id: #UInt16)
  returns (ret: #OperandId)
{
  var $operandId'v0: #OperandId;
  var $tmp'v1: #UInt16;
  
  $operandId'v0 := #OperandId~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id($operandId'v0);
  assume ($tmp'v1 == $id);
  ret := $operandId'v0;
  return;
}

function #OperandId~fromIdUnchecked($id: #UInt16): #OperandId;

procedure #OperandId~toValueId($operandId: #OperandId)
  returns (ret: #ValueId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id($operandId);
  call ret := #ValueId~fromId($tmp'v0);
  return;
}

procedure #OperandId~toObjectId($operandId: #OperandId)
  returns (ret: #ObjectId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id($operandId);
  call ret := #ObjectId~fromId($tmp'v0);
  return;
}

procedure #OperandId~toStringId($operandId: #OperandId)
  returns (ret: #StringId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id($operandId);
  call ret := #StringId~fromId($tmp'v0);
  return;
}

procedure #OperandId~toSymbolId($operandId: #OperandId)
  returns (ret: #SymbolId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id($operandId);
  call ret := #SymbolId~fromId($tmp'v0);
  return;
}

procedure #OperandId~toBoolId($operandId: #OperandId)
  returns (ret: #BoolId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id($operandId);
  call ret := #BoolId~fromId($tmp'v0);
  return;
}

procedure #OperandId~toInt32Id($operandId: #OperandId)
  returns (ret: #Int32Id)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id($operandId);
  call ret := #Int32Id~fromId($tmp'v0);
  return;
}

procedure #OperandId~toNumberId($operandId: #OperandId)
  returns (ret: #NumberId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id($operandId);
  call ret := #NumberId~fromId($tmp'v0);
  return;
}

procedure #OperandId~toBigIntId($operandId: #OperandId)
  returns (ret: #BigIntId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id($operandId);
  call ret := #BigIntId~fromId($tmp'v0);
  return;
}

procedure #OperandId~toValueTagId($operandId: #OperandId)
  returns (ret: #ValueTagId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id($operandId);
  call ret := #ValueTagId~fromId($tmp'v0);
  return;
}

procedure #OperandId~toIntPtrId($operandId: #OperandId)
  returns (ret: #IntPtrId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id($operandId);
  call ret := #IntPtrId~fromId($tmp'v0);
  return;
}

procedure #OperandId~toRawId($operandId: #OperandId)
  returns (ret: #RawId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id($operandId);
  call ret := #RawId~fromId($tmp'v0);
  return;
}

procedure #ValueId~fromId($id: #UInt16)
  returns (ret: #ValueId)
{
  var $valueId'v0: #ValueId;
  var $tmp'v1: #UInt16;
  
  $valueId'v0 := #ValueId~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id(#ValueId^to#OperandId($valueId'v0));
  assume ($tmp'v1 == $id);
  ret := $valueId'v0;
  return;
}

function #ValueId~fromIdUnchecked($id: #UInt16): #ValueId;

procedure #ObjectId~fromId($id: #UInt16)
  returns (ret: #ObjectId)
{
  var $objectId'v0: #ObjectId;
  var $tmp'v1: #UInt16;
  
  $objectId'v0 := #ObjectId~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id(#ObjectId^to#OperandId($objectId'v0));
  assume ($tmp'v1 == $id);
  ret := $objectId'v0;
  return;
}

function #ObjectId~fromIdUnchecked($id: #UInt16): #ObjectId;

procedure #StringId~fromId($id: #UInt16)
  returns (ret: #StringId)
{
  var $stringId'v0: #StringId;
  var $tmp'v1: #UInt16;
  
  $stringId'v0 := #StringId~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id(#StringId^to#OperandId($stringId'v0));
  assume ($tmp'v1 == $id);
  ret := $stringId'v0;
  return;
}

function #StringId~fromIdUnchecked($id: #UInt16): #StringId;

procedure #SymbolId~fromId($id: #UInt16)
  returns (ret: #SymbolId)
{
  var $symbolId'v0: #SymbolId;
  var $tmp'v1: #UInt16;
  
  $symbolId'v0 := #SymbolId~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id(#SymbolId^to#OperandId($symbolId'v0));
  assume ($tmp'v1 == $id);
  ret := $symbolId'v0;
  return;
}

function #SymbolId~fromIdUnchecked($id: #UInt16): #SymbolId;

procedure #BoolId~fromId($id: #UInt16)
  returns (ret: #BoolId)
{
  var $boolId'v0: #BoolId;
  var $tmp'v1: #UInt16;
  
  $boolId'v0 := #BoolId~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id(#BoolId^to#OperandId($boolId'v0));
  assume ($tmp'v1 == $id);
  ret := $boolId'v0;
  return;
}

function #BoolId~fromIdUnchecked($id: #UInt16): #BoolId;

procedure #Int32Id~fromId($id: #UInt16)
  returns (ret: #Int32Id)
{
  var $int32Id'v0: #Int32Id;
  var $tmp'v1: #UInt16;
  
  $int32Id'v0 := #Int32Id~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id(#Int32Id^to#OperandId($int32Id'v0));
  assume ($tmp'v1 == $id);
  ret := $int32Id'v0;
  return;
}

function #Int32Id~fromIdUnchecked($id: #UInt16): #Int32Id;

procedure #NumberId~fromId($id: #UInt16)
  returns (ret: #NumberId)
{
  var $numberId'v0: #NumberId;
  var $tmp'v1: #UInt16;
  
  $numberId'v0 := #NumberId~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id(#ValueId^to#OperandId(#NumberId^to#ValueId($numberId'v0)));
  assume ($tmp'v1 == $id);
  ret := $numberId'v0;
  return;
}

function #NumberId~fromIdUnchecked($id: #UInt16): #NumberId;

procedure #BigIntId~fromId($id: #UInt16)
  returns (ret: #BigIntId)
{
  var $bigIntId'v0: #BigIntId;
  var $tmp'v1: #UInt16;
  
  $bigIntId'v0 := #BigIntId~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id(#BigIntId^to#OperandId($bigIntId'v0));
  assume ($tmp'v1 == $id);
  ret := $bigIntId'v0;
  return;
}

function #BigIntId~fromIdUnchecked($id: #UInt16): #BigIntId;

procedure #ValueTagId~fromId($id: #UInt16)
  returns (ret: #ValueTagId)
{
  var $valueTagId'v0: #ValueTagId;
  var $tmp'v1: #UInt16;
  
  $valueTagId'v0 := #ValueTagId~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id(#ValueTagId^to#OperandId($valueTagId'v0));
  assume ($tmp'v1 == $id);
  ret := $valueTagId'v0;
  return;
}

function #ValueTagId~fromIdUnchecked($id: #UInt16): #ValueTagId;

procedure #IntPtrId~fromId($id: #UInt16)
  returns (ret: #IntPtrId)
{
  var $intPtrId'v0: #IntPtrId;
  var $tmp'v1: #UInt16;
  
  $intPtrId'v0 := #IntPtrId~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id(#IntPtrId^to#OperandId($intPtrId'v0));
  assume ($tmp'v1 == $id);
  ret := $intPtrId'v0;
  return;
}

function #IntPtrId~fromIdUnchecked($id: #UInt16): #IntPtrId;

procedure #RawId~fromId($id: #UInt16)
  returns (ret: #RawId)
{
  var $rawId'v0: #RawId;
  var $tmp'v1: #UInt16;
  
  $rawId'v0 := #RawId~fromIdUnchecked($id);
  $tmp'v1 := #OperandId~id(#RawId^to#OperandId($rawId'v0));
  assume ($tmp'v1 == $id);
  ret := $rawId'v0;
  return;
}

function #RawId~fromIdUnchecked($id: #UInt16): #RawId;

function #TypedId~type($typedId: #TypedId): #ValueType;

procedure #TypedId~new($id: #UInt16, $type: #ValueType)
  returns (ret: #TypedId)
{
  var $typedId'v0: #TypedId;
  var $tmp'v1: #UInt16;
  var $tmp'v2: #ValueType;
  
  $typedId'v0 := #TypedId~newUnchecked($id, $type);
  $tmp'v1 := #OperandId~id(#TypedId^to#OperandId($typedId'v0));
  assume ($tmp'v1 == $id);
  $tmp'v2 := #TypedId~type($typedId'v0);
  assume ($tmp'v2 == $type);
  ret := $typedId'v0;
  return;
}

function #TypedId~newUnchecked($id: #UInt16, $type: #ValueType): #TypedId;

procedure #TypedId~fromObjectId($objectId: #ObjectId)
  returns (ret: #TypedId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id(#ObjectId^to#OperandId($objectId));
  call ret := #TypedId~new($tmp'v0, #ValueType^Variant~Object());
  return;
}

procedure #TypedId~toObjectId($typedId: #TypedId)
  returns (ret: #ObjectId)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #TypedId~type($typedId);
  assert ($tmp'v0 == #ValueType^Variant~Object());
  call ret := #OperandId~toObjectId(#TypedId^to#OperandId($typedId));
  return;
}

procedure #TypedId~fromStringId($stringId: #StringId)
  returns (ret: #TypedId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id(#StringId^to#OperandId($stringId));
  call ret := #TypedId~new($tmp'v0, #ValueType^Variant~String());
  return;
}

procedure #TypedId~toStringId($typedId: #TypedId)
  returns (ret: #StringId)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #TypedId~type($typedId);
  assert ($tmp'v0 == #ValueType^Variant~String());
  call ret := #OperandId~toStringId(#TypedId^to#OperandId($typedId));
  return;
}

procedure #TypedId~fromSymbolId($symbolId: #SymbolId)
  returns (ret: #TypedId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id(#SymbolId^to#OperandId($symbolId));
  call ret := #TypedId~new($tmp'v0, #ValueType^Variant~Symbol());
  return;
}

procedure #TypedId~toSymbolId($typedId: #TypedId)
  returns (ret: #SymbolId)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #TypedId~type($typedId);
  assert ($tmp'v0 == #ValueType^Variant~Symbol());
  call ret := #OperandId~toSymbolId(#TypedId^to#OperandId($typedId));
  return;
}

procedure #TypedId~fromBoolId($boolId: #BoolId)
  returns (ret: #TypedId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id(#BoolId^to#OperandId($boolId));
  call ret := #TypedId~new($tmp'v0, #ValueType^Variant~Bool());
  return;
}

procedure #TypedId~toBoolId($typedId: #TypedId)
  returns (ret: #BoolId)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #TypedId~type($typedId);
  assert ($tmp'v0 == #ValueType^Variant~Bool());
  call ret := #OperandId~toBoolId(#TypedId^to#OperandId($typedId));
  return;
}

procedure #TypedId~fromInt32Id($int32Id: #Int32Id)
  returns (ret: #TypedId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id(#Int32Id^to#OperandId($int32Id));
  call ret := #TypedId~new($tmp'v0, #ValueType^Variant~Int32());
  return;
}

procedure #TypedId~toInt32Id($typedId: #TypedId)
  returns (ret: #Int32Id)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #TypedId~type($typedId);
  assert ($tmp'v0 == #ValueType^Variant~Int32());
  call ret := #OperandId~toInt32Id(#TypedId^to#OperandId($typedId));
  return;
}

procedure #TypedId~fromBigIntId($bigIntId: #BigIntId)
  returns (ret: #TypedId)
{
  var $tmp'v0: #UInt16;
  
  $tmp'v0 := #OperandId~id(#BigIntId^to#OperandId($bigIntId));
  call ret := #TypedId~new($tmp'v0, #ValueType^Variant~BigInt());
  return;
}

procedure #TypedId~toBigIntId($typedId: #TypedId)
  returns (ret: #BigIntId)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #TypedId~type($typedId);
  assert ($tmp'v0 == #ValueType^Variant~BigInt());
  call ret := #OperandId~toBigIntId(#TypedId^to#OperandId($typedId));
  return;
}

function #Int32Field~fromOffset($offset: #UInt32): #Int32Field;

function #IntPtrField~fromOffset($offset: #UInt32): #IntPtrField;

function #ShapeField~fromOffset($offset: #UInt32): #ShapeField;

function #ClassField~fromOffset($offset: #UInt32): #ClassField;

function #GetterSetterField~fromOffset($offset: #UInt32): #GetterSetterField;

function #ObjectField~fromOffset($offset: #UInt32): #ObjectField;

function #SymbolField~fromOffset($offset: #UInt32): #SymbolField;

function #StringField~fromOffset($offset: #UInt32): #StringField;

function #BaseScriptField~fromOffset($offset: #UInt32): #BaseScriptField;

function #IdField~fromOffset($offset: #UInt32): #IdField;

function #AllocSiteField~fromOffset($offset: #UInt32): #AllocSiteField;

function #Int64Field~fromOffset($offset: #UInt32): #Int64Field;

function #ValueField~fromOffset($offset: #UInt32): #ValueField;

procedure {:inline 1} #CacheIR~addFailurePath($failure: EmitPath)
{
  call #MASM^bindExit($failure);
}

procedure #CacheIR~defineObjectId($id: #ObjectId)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromObjectId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineTypedId($typedId'v0);
  return;
}

procedure #CacheIR~defineInt32Id($id: #Int32Id)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromInt32Id($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineTypedId($typedId'v0);
  return;
}

procedure #CacheIR~defineNumberId($id: #NumberId)
  returns (ret: #ValueReg)
{
  var $valueId'v0: #ValueId;
  
  $valueId'v0 := #NumberId^to#ValueId($id);
  call ret := #CacheIR~defineValueId($valueId'v0);
  return;
}

procedure #CacheIR~defineBoolId($id: #BoolId)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromBoolId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineTypedId($typedId'v0);
  return;
}

procedure #CacheIR~defineStringId($id: #StringId)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromStringId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineTypedId($typedId'v0);
  return;
}

procedure #CacheIR~defineSymbolId($id: #SymbolId)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromSymbolId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineTypedId($typedId'v0);
  return;
}

procedure #CacheIR~defineBigIntId($id: #BigIntId)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromBigIntId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineTypedId($typedId'v0);
  return;
}

procedure #CacheIR~useTypedId($id: #TypedId, emitPath: EmitPath)
  returns (ret: #Reg)
{
  var $operandId'v0: #OperandId;
  var $location'v1: #OperandLocation;
  var $locationKind'v2: #OperandLocationKind;
  var $valueReg'v3: #ValueReg;
  var $reg'v4: #Reg;
  var $tmp'v5: #ValueType;
  var $tmp'v6: #ValueType;
  var out'0: #OperandLocation;
  var out'1: #ValueReg;
  var out'2: #Reg;
  
  $operandId'v0 := #TypedId^to#OperandId($id);
  call out'0 := #CacheIR~getOperandLocation($operandId'v0);
  $location'v1 := out'0;
  $locationKind'v2 := #OperandLocation~kind($location'v1);
  if ($locationKind'v2 == #OperandLocationKind^Variant~PayloadReg()) {
    call ret := #OperandLocation~getPayloadReg($location'v1);
    return;
  } else if ($locationKind'v2 == #OperandLocationKind^Variant~ValueReg()) {
    call out'1 := #OperandLocation~getValueReg($location'v1);
    $valueReg'v3 := out'1;
    call #CacheIR~releaseValueReg($valueReg'v3);
    call out'2 := #ValueReg~scratchReg($valueReg'v3);
    $reg'v4 := out'2;
    call #CacheIR~allocateKnownReg($reg'v4);
    $tmp'v5 := #TypedId~type($id);
    call #MASM^emit(#MASM^Op~UnboxNonDouble($valueReg'v3, $reg'v4, $tmp'v5), ConsEmitPath(emitPath, 0));
    $tmp'v6 := #TypedId~type($id);
    call $location'v1 := #OperandLocation~setPayloadReg($reg'v4, $tmp'v6);
    call #CacheIR~setOperandLocation($operandId'v0, $location'v1);
    ret := $reg'v4;
    return;
  } else {
    assert false;
  }
  call ret := #OperandLocation~getPayloadReg($location'v1);
  return;
}

procedure #CacheIR~useValueId($valueId: #ValueId, emitPath: EmitPath)
  returns (ret: #ValueReg)
{
  var $operandId'v0: #OperandId;
  var $location'v1: #OperandLocation;
  var $locationKind'v2: #OperandLocationKind;
  var $reg'v3: #Reg;
  var $valTy'v4: #ValueType;
  var $valueReg'v5: #ValueReg;
  var out'0: #OperandLocation;
  var out'1: #Reg;
  var out'2: #ValueType;
  var out'3: #ValueReg;
  
  $operandId'v0 := #ValueId^to#OperandId($valueId);
  call out'0 := #CacheIR~getOperandLocation($operandId'v0);
  $location'v1 := out'0;
  $locationKind'v2 := #OperandLocation~kind($location'v1);
  if ($locationKind'v2 == #OperandLocationKind^Variant~ValueReg()) {
    call ret := #OperandLocation~getValueReg($location'v1);
    return;
  } else if ($locationKind'v2 == #OperandLocationKind^Variant~PayloadReg()) {
    call out'1 := #OperandLocation~getPayloadReg($location'v1);
    $reg'v3 := out'1;
    call out'2 := #OperandLocation~getPayloadType($location'v1);
    $valTy'v4 := out'2;
    call out'3 := #CacheIR~allocateValueReg();
    $valueReg'v5 := out'3;
    call #MASM^emit(#MASM^Op~TagValue($valTy'v4, $reg'v3, $valueReg'v5), ConsEmitPath(emitPath, 0));
    call $location'v1 := #OperandLocation~setValueReg($valueReg'v5);
    call #CacheIR~releaseReg($reg'v3);
    ret := $valueReg'v5;
    return;
  } else {
    assert false;
  }
  call ret := #OperandLocation~getValueReg($location'v1);
  return;
}

procedure #CacheIR~useObjectId($id: #ObjectId, emitPath: EmitPath)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromObjectId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useTypedId($typedId'v0, ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~useInt32Id($id: #Int32Id, emitPath: EmitPath)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromInt32Id($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useTypedId($typedId'v0, ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~useNumberId($id: #NumberId, emitPath: EmitPath)
  returns (ret: #ValueReg)
{
  var $valueId'v0: #ValueId;
  
  $valueId'v0 := #NumberId^to#ValueId($id);
  call ret := #CacheIR~useValueId($valueId'v0, ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~useBoolId($id: #BoolId, emitPath: EmitPath)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromBoolId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useTypedId($typedId'v0, ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~useStringId($id: #StringId, emitPath: EmitPath)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromStringId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useTypedId($typedId'v0, ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~useSymbolId($id: #SymbolId, emitPath: EmitPath)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromSymbolId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useTypedId($typedId'v0, ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~useBigIntId($id: #BigIntId, emitPath: EmitPath)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromBigIntId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useTypedId($typedId'v0, ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~emitLoadInt32StubField($int32Field: #Int32Field, $dstReg: #Reg, emitPath: EmitPath)
{
  var $tmp'v0: #Int32;
  
  $tmp'v0 := #CacheIR~readInt32Field($int32Field);
  call #MASM^emit(#MASM^Op~Move32Imm32($tmp'v0, $dstReg), ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~emitLoadValueStubField($valueField: #ValueField, $dstReg: #ValueReg, emitPath: EmitPath)
{
  var $tmp'v0: #Value;
  
  $tmp'v0 := #CacheIR~readValueField($valueField);
  call #MASM^emit(#MASM^Op~MoveValueImm($tmp'v0, $dstReg), ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~emitLoadObjectStubField($objectField: #ObjectField, $dstReg: #Reg, emitPath: EmitPath)
{
  var $tmp'v0: #Object;
  
  $tmp'v0 := #CacheIR~readObjectField($objectField);
  call #MASM^emit(#MASM^Op~MovePtrImmGCPtrObject($tmp'v0, $dstReg), ConsEmitPath(emitPath, 0));
  return;
}

function #CacheIR~readInt32Field($int32Field: #Int32Field): #Int32;

function #CacheIR~readShapeField($shapeField: #ShapeField): #Shape;

function #CacheIR~readClassField($classField: #ClassField): #Class;

function #CacheIR~readObjectField($objectField: #ObjectField): #Object;

function #CacheIR~readInt64Field($int64Field: #Int64Field): #Int64;

function #CacheIR~readValueField($valueField: #ValueField): #Value;

function #CacheIR~objectGuardNeedsSpectreMitigations($objectId: #ObjectId): #Bool;

procedure #initInputObjectId($objectId: #ObjectId)
{
  var $reg'v0: #Reg;
  var $data'v1: #RegData;
  var $tmp'v2: #TypedId;
  var $tmp'v3: #Value;
  var out'0: #TypedId;
  var out'1: #Reg;
  var out'2: #RegData;
  var out'3: #Bool;
  var out'4: #Value;
  var out'5: #Bool;
  
  call out'0 := #TypedId~fromObjectId($objectId);
  $tmp'v2 := out'0;
  call out'1 := #CacheIR~defineTypedId($tmp'v2);
  $reg'v0 := out'1;
  call out'2 := #MASM~getData($reg'v0);
  $data'v1 := out'2;
  call out'3 := #RegData~isUnboxedValue($data'v1);
  assume out'3;
  call out'4 := #RegData~toUnboxedValue($data'v1);
  $tmp'v3 := out'4;
  call out'5 := #Value~isObject($tmp'v3);
  assume out'5;
  return;
}

procedure #initInputInt32Id($int32Id: #Int32Id)
{
  var $reg'v0: #Reg;
  var $data'v1: #RegData;
  var $tmp'v2: #TypedId;
  var $tmp'v3: #Value;
  var out'0: #TypedId;
  var out'1: #Reg;
  var out'2: #RegData;
  var out'3: #Bool;
  var out'4: #Value;
  var out'5: #Bool;
  
  call out'0 := #TypedId~fromInt32Id($int32Id);
  $tmp'v2 := out'0;
  call out'1 := #CacheIR~defineTypedId($tmp'v2);
  $reg'v0 := out'1;
  call out'2 := #MASM~getData($reg'v0);
  $data'v1 := out'2;
  call out'3 := #RegData~isUnboxedValue($data'v1);
  assume out'3;
  call out'4 := #RegData~toUnboxedValue($data'v1);
  $tmp'v3 := out'4;
  call out'5 := #Value~isInt32($tmp'v3);
  assume out'5;
  return;
}

procedure #initInputNumberId($numberId: #NumberId)
{
  var $tmp'v0: #ValueId;
  var out'0: #ValueId;
  
  call out'0 := #OperandId~toValueId(#ValueId^to#OperandId(#NumberId^to#ValueId($numberId)));
  $tmp'v0 := out'0;
  call #initInputValueId($tmp'v0);
  return;
}

procedure #initInputBoolId($boolId: #BoolId)
{
  var $reg'v0: #Reg;
  var $data'v1: #RegData;
  var $tmp'v2: #TypedId;
  var $tmp'v3: #Value;
  var out'0: #TypedId;
  var out'1: #Reg;
  var out'2: #RegData;
  var out'3: #Bool;
  var out'4: #Value;
  var out'5: #Bool;
  
  call out'0 := #TypedId~fromBoolId($boolId);
  $tmp'v2 := out'0;
  call out'1 := #CacheIR~defineTypedId($tmp'v2);
  $reg'v0 := out'1;
  call out'2 := #MASM~getData($reg'v0);
  $data'v1 := out'2;
  call out'3 := #RegData~isUnboxedValue($data'v1);
  assume out'3;
  call out'4 := #RegData~toUnboxedValue($data'v1);
  $tmp'v3 := out'4;
  call out'5 := #Value~isBool($tmp'v3);
  assume out'5;
  return;
}

procedure #initInputStringId($stringId: #StringId)
{
  var $reg'v0: #Reg;
  var $data'v1: #RegData;
  var $tmp'v2: #TypedId;
  var $tmp'v3: #Value;
  var out'0: #TypedId;
  var out'1: #Reg;
  var out'2: #RegData;
  var out'3: #Bool;
  var out'4: #Value;
  var out'5: #Bool;
  
  call out'0 := #TypedId~fromStringId($stringId);
  $tmp'v2 := out'0;
  call out'1 := #CacheIR~defineTypedId($tmp'v2);
  $reg'v0 := out'1;
  call out'2 := #MASM~getData($reg'v0);
  $data'v1 := out'2;
  call out'3 := #RegData~isUnboxedValue($data'v1);
  assume out'3;
  call out'4 := #RegData~toUnboxedValue($data'v1);
  $tmp'v3 := out'4;
  call out'5 := #Value~isString($tmp'v3);
  assume out'5;
  return;
}

procedure #initInputSymbolId($symbolId: #SymbolId)
{
  var $reg'v0: #Reg;
  var $data'v1: #RegData;
  var $tmp'v2: #TypedId;
  var $tmp'v3: #Value;
  var out'0: #TypedId;
  var out'1: #Reg;
  var out'2: #RegData;
  var out'3: #Bool;
  var out'4: #Value;
  var out'5: #Bool;
  
  call out'0 := #TypedId~fromSymbolId($symbolId);
  $tmp'v2 := out'0;
  call out'1 := #CacheIR~defineTypedId($tmp'v2);
  $reg'v0 := out'1;
  call out'2 := #MASM~getData($reg'v0);
  $data'v1 := out'2;
  call out'3 := #RegData~isUnboxedValue($data'v1);
  assume out'3;
  call out'4 := #RegData~toUnboxedValue($data'v1);
  $tmp'v3 := out'4;
  call out'5 := #Value~isSymbol($tmp'v3);
  assume out'5;
  return;
}

procedure #initInputBigIntId($bigIntId: #BigIntId)
{
  var $reg'v0: #Reg;
  var $data'v1: #RegData;
  var $tmp'v2: #TypedId;
  var $tmp'v3: #Value;
  var out'0: #TypedId;
  var out'1: #Reg;
  var out'2: #RegData;
  var out'3: #Bool;
  var out'4: #Value;
  var out'5: #Bool;
  
  call out'0 := #TypedId~fromBigIntId($bigIntId);
  $tmp'v2 := out'0;
  call out'1 := #CacheIR~defineTypedId($tmp'v2);
  $reg'v0 := out'1;
  call out'2 := #MASM~getData($reg'v0);
  $data'v1 := out'2;
  call out'3 := #RegData~isUnboxedValue($data'v1);
  assume out'3;
  call out'4 := #RegData~toUnboxedValue($data'v1);
  $tmp'v3 := out'4;
  call out'5 := #Value~isBigInt($tmp'v3);
  assume out'5;
  return;
}

function {:constructor} #MASM^Op~AssumeUnreachable(): #MASM^Op;

procedure #MASM~AssumeUnreachable()
{
  assert false;
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Mov($srcReg: #Reg, $dstReg: #Reg): #MASM^Op;

procedure #MASM~Mov($srcReg: #Reg, $dstReg: #Reg)
{
  var $tmp'v0: #Int32;
  var out'0: #Int32;
  
  call out'0 := #MASM~getInt32($srcReg);
  $tmp'v0 := out'0;
  call #MASM~setInt32($dstReg, $tmp'v0);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Move32Imm32($srcInt32: #Int32, $dstReg: #Reg): #MASM^Op;

procedure #MASM~Move32Imm32($srcInt32: #Int32, $dstReg: #Reg)
{
  call #MASM~setInt32($dstReg, $srcInt32);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~MoveValue($srcReg: #ValueReg, $dstReg: #ValueReg): #MASM^Op;

procedure #MASM~MoveValue($srcReg: #ValueReg, $dstReg: #ValueReg)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #MASM~getValue($srcReg);
  $tmp'v0 := out'0;
  call #MASM~setValue($dstReg, $tmp'v0);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~MoveValueImm($value: #Value, $dstReg: #ValueReg): #MASM^Op;

procedure #MASM~MoveValueImm($value: #Value, $dstReg: #ValueReg)
{
  call #MASM~setValue($dstReg, $value);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~MovePtrImmGCPtrObject($object: #Object, $dstReg: #Reg): #MASM^Op;

procedure #MASM~MovePtrImmGCPtrObject($object: #Object, $dstReg: #Reg)
{
  call #MASM~setObject($dstReg, $object);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Cmp32Move32($condition: #Condition, $lhsReg: #Reg, $rhsReg: #Reg, $srcReg: #Reg, $dstReg: #Reg): #MASM^Op;

procedure #MASM~Cmp32Move32($condition: #Condition, $lhsReg: #Reg, $rhsReg: #Reg, $srcReg: #Reg, $dstReg: #Reg)
{
  var $lhsInt32'v0: #Int32;
  var $rhsInt32'v1: #Int32;
  var $srcInt32'v2: #Int32;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var out'0: #Int32;
  var out'1: #Int32;
  var out'2: #Int32;
  
  $tmp'v3 := true;
  if (!($condition == #Condition^Variant~GreaterThan())) {
    $tmp'v3 := ($condition == #Condition^Variant~LessThan());
  }
  assert $tmp'v3;
  call out'0 := #MASM~getInt32($lhsReg);
  $lhsInt32'v0 := out'0;
  call out'1 := #MASM~getInt32($rhsReg);
  $rhsInt32'v1 := out'1;
  call out'2 := #MASM~getInt32($srcReg);
  $srcInt32'v2 := out'2;
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~GreaterThan()) {
    $tmp'v4 := #Int32^gt($lhsInt32'v0, $rhsInt32'v1);
  }
  $tmp'v5 := false;
  if ($condition == #Condition^Variant~LessThan()) {
    $tmp'v5 := #Int32^lt($lhsInt32'v0, $rhsInt32'v1);
  }
  if ($tmp'v4) {
    call #MASM~setInt32($dstReg, $srcInt32'v2);
  } else if ($tmp'v5) {
    call #MASM~setInt32($dstReg, $srcInt32'v2);
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~StoreBool($bool: #Bool, $reg: #Reg): #MASM^Op;

procedure #MASM~StoreBool($bool: #Bool, $reg: #Reg)
{
  call #MASM~setBool($reg, $bool);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~StoreBoolValue($bool: #Bool, $valueReg: #ValueReg): #MASM^Op;

procedure #MASM~StoreBoolValue($bool: #Bool, $valueReg: #ValueReg)
{
  var $tmp'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #Value~fromBool($bool);
  $tmp'v0 := out'0;
  call #MASM~setValue($valueReg, $tmp'v0);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Jump($target: EmitPath): #MASM^Op;

procedure #MASM~Jump($target: EmitPath)
{
  call #MASM^goto($target);
  return;
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Load32($int32: #Int32, $dstReg: #Reg): #MASM^Op;

procedure #MASM~Load32($int32: #Int32, $dstReg: #Reg)
{
  call #MASM~setInt32($dstReg, $int32);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Load32Address($address: #Address, $dstReg: #Reg): #MASM^Op;

procedure #MASM~Load32Address($address: #Address, $dstReg: #Reg)
{
  var $baseData'v0: #RegData;
  var $data'v1: #RegData;
  var $value'v2: #Value;
  var $int32'v3: #Int32;
  var out'0: #RegData;
  var out'1: #RegData;
  var out'2: #Value;
  var out'3: #Int32;
  
  call out'0 := #MASM~getData(#Address^field~base($address));
  $baseData'v0 := out'0;
  call out'1 := #RegData~readData($baseData'v0, #Int32^to#Int64(#Address^field~offset($address)));
  $data'v1 := out'1;
  call out'2 := #RegData~toValue($data'v1);
  $value'v2 := out'2;
  call out'3 := #Value~toInt32($value'v2);
  $int32'v3 := out'3;
  call #MASM~setInt32($dstReg, $int32'v3);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~LoadValueAddress($address: #Address, $dstReg: #ValueReg): #MASM^Op;

procedure #MASM~LoadValueAddress($address: #Address, $dstReg: #ValueReg)
{
  var $baseData'v0: #RegData;
  var $data'v1: #RegData;
  var $value'v2: #Value;
  var out'0: #RegData;
  var out'1: #RegData;
  var out'2: #Value;
  
  call out'0 := #MASM~getData(#Address^field~base($address));
  $baseData'v0 := out'0;
  call out'1 := #RegData~readData($baseData'v0, #Int32^to#Int64(#Address^field~offset($address)));
  $data'v1 := out'1;
  call out'2 := #RegData~toValue($data'v1);
  $value'v2 := out'2;
  call #MASM~setValue($dstReg, $value'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~LoadPtrAddress($address: #Address, $dstReg: #Reg): #MASM^Op;

procedure #MASM~LoadPtrAddress($address: #Address, $dstReg: #Reg)
{
  var $baseData'v0: #RegData;
  var $data'v1: #RegData;
  var out'0: #RegData;
  var out'1: #RegData;
  
  call out'0 := #MASM~getData(#Address^field~base($address));
  $baseData'v0 := out'0;
  call out'1 := #RegData~readData($baseData'v0, #Int32^to#Int64(#Address^field~offset($address)));
  $data'v1 := out'1;
  call #MASM~setData($dstReg, $data'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~TagValue($valTy: #ValueType, $payload: #Reg, $dest: #ValueReg): #MASM^Op;

procedure #MASM~TagValue($valTy: #ValueType, $payload: #Reg, $dest: #ValueReg)
{
  var $i'v0: #Int32;
  var $val'v1: #Value;
  var $b'v2: #Bool;
  var $val'v3: #Value;
  var $s'v4: #String;
  var $val'v5: #Value;
  var $s'v6: #Symbol;
  var $val'v7: #Value;
  var $b'v8: #BigInt;
  var $val'v9: #Value;
  var out'0: #Int32;
  var out'1: #Value;
  var out'2: #Bool;
  var out'3: #Value;
  var out'4: #String;
  var out'5: #Value;
  var out'6: #Symbol;
  var out'7: #Value;
  var out'8: #BigInt;
  var out'9: #Value;
  
  if ($valTy == #ValueType^Variant~Int32()) {
    call out'0 := #MASM~getInt32($payload);
    $i'v0 := out'0;
    call out'1 := #Value~fromInt32($i'v0);
    $val'v1 := out'1;
    call #MASM~setValue($dest, $val'v1);
  } else if ($valTy == #ValueType^Variant~Bool()) {
    call out'2 := #MASM~getBool($payload);
    $b'v2 := out'2;
    call out'3 := #Value~fromBool($b'v2);
    $val'v3 := out'3;
    call #MASM~setValue($dest, $val'v3);
  } else if ($valTy == #ValueType^Variant~String()) {
    call out'4 := #MASM~getString($payload);
    $s'v4 := out'4;
    call out'5 := #Value~fromString($s'v4);
    $val'v5 := out'5;
    call #MASM~setValue($dest, $val'v5);
  } else if ($valTy == #ValueType^Variant~Symbol()) {
    call out'6 := #MASM~getSymbol($payload);
    $s'v6 := out'6;
    call out'7 := #Value~fromSymbol($s'v6);
    $val'v7 := out'7;
    call #MASM~setValue($dest, $val'v7);
  } else if ($valTy == #ValueType^Variant~BigInt()) {
    call out'8 := #MASM~getBigInt($payload);
    $b'v8 := out'8;
    call out'9 := #Value~fromBigInt($b'v8);
    $val'v9 := out'9;
    call #MASM~setValue($dest, $val'v9);
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~UnboxInt32($valueReg: #ValueReg, $int32Reg: #Reg): #MASM^Op;

procedure #MASM~UnboxInt32($valueReg: #ValueReg, $int32Reg: #Reg)
{
  var $value'v0: #Value;
  var $tmp'v1: #Int32;
  var out'0: #Value;
  var out'1: #Int32;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~toInt32($value'v0);
  $tmp'v1 := out'1;
  call #MASM~setInt32($int32Reg, $tmp'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~UnboxBoolean($valueReg: #ValueReg, $boolReg: #Reg): #MASM^Op;

procedure #MASM~UnboxBoolean($valueReg: #ValueReg, $boolReg: #Reg)
{
  var $value'v0: #Value;
  var $tmp'v1: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~toBool($value'v0);
  $tmp'v1 := out'1;
  call #MASM~setBool($boolReg, $tmp'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~UnboxObject($valueReg: #ValueReg, $objectReg: #Reg): #MASM^Op;

procedure #MASM~UnboxObject($valueReg: #ValueReg, $objectReg: #Reg)
{
  var $value'v0: #Value;
  var $tmp'v1: #Object;
  var out'0: #Value;
  var out'1: #Object;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~toObject($value'v0);
  $tmp'v1 := out'1;
  call #MASM~setObject($objectReg, $tmp'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~UnboxObjectProto($srcReg: #Reg, $destReg: #Reg): #MASM^Op;

procedure #MASM~UnboxObjectProto($srcReg: #Reg, $destReg: #Reg)
{
  var $data'v0: #RegData;
  var $proto'v1: #TaggedProto;
  var $object'v2: #Object;
  var out'0: #RegData;
  var out'1: #TaggedProto;
  var out'2: #Object;
  
  call out'0 := #MASM~getData($srcReg);
  $data'v0 := out'0;
  call out'1 := #RegData~toTaggedProto($data'v0);
  $proto'v1 := out'1;
  call out'2 := #TaggedProto~toObject($proto'v1);
  $object'v2 := out'2;
  call #MASM~setObject($destReg, $object'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~FallibleUnboxObject($valueReg: #ValueReg, $objectReg: #Reg, $failure: EmitPath): #MASM^Op;

procedure #MASM~FallibleUnboxObject($valueReg: #ValueReg, $objectReg: #Reg, $failure: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsObject'v1: #Bool;
  var $object'v2: #Object;
  var out'0: #Value;
  var out'1: #Bool;
  var out'2: #Object;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isObject($value'v0);
  $valueIsObject'v1 := out'1;
  if (!$valueIsObject'v1) {
    call #MASM^goto($failure);
    return;
  }
  call out'2 := #Value~toObject($value'v0);
  $object'v2 := out'2;
  call #MASM~setObject($objectReg, $object'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~FallibleUnboxBoolean($valueReg: #ValueReg, $boolReg: #Reg, $failure: EmitPath): #MASM^Op;

procedure #MASM~FallibleUnboxBoolean($valueReg: #ValueReg, $boolReg: #Reg, $failure: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsBool'v1: #Bool;
  var $bool'v2: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  var out'2: #Bool;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isBool($value'v0);
  $valueIsBool'v1 := out'1;
  if (!$valueIsBool'v1) {
    call #MASM^goto($failure);
    return;
  }
  call out'2 := #Value~toBool($value'v0);
  $bool'v2 := out'2;
  call #MASM~setBool($boolReg, $bool'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~UnboxString($valueReg: #ValueReg, $stringReg: #Reg): #MASM^Op;

procedure #MASM~UnboxString($valueReg: #ValueReg, $stringReg: #Reg)
{
  var $value'v0: #Value;
  var $tmp'v1: #String;
  var out'0: #Value;
  var out'1: #String;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~toString($value'v0);
  $tmp'v1 := out'1;
  call #MASM~setString($stringReg, $tmp'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~UnboxSymbol($valueReg: #ValueReg, $symbolReg: #Reg): #MASM^Op;

procedure #MASM~UnboxSymbol($valueReg: #ValueReg, $symbolReg: #Reg)
{
  var $value'v0: #Value;
  var $tmp'v1: #Symbol;
  var out'0: #Value;
  var out'1: #Symbol;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~toSymbol($value'v0);
  $tmp'v1 := out'1;
  call #MASM~setSymbol($symbolReg, $tmp'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~UnboxBigInt($valueReg: #ValueReg, $bigIntReg: #Reg): #MASM^Op;

procedure #MASM~UnboxBigInt($valueReg: #ValueReg, $bigIntReg: #Reg)
{
  var $value'v0: #Value;
  var $tmp'v1: #BigInt;
  var out'0: #Value;
  var out'1: #BigInt;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~toBigInt($value'v0);
  $tmp'v1 := out'1;
  call #MASM~setBigInt($bigIntReg, $tmp'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~UnboxNonDouble($valueReg: #ValueReg, $dstReg: #Reg, $valTy: #ValueType): #MASM^Op;

procedure #MASM~UnboxNonDouble($valueReg: #ValueReg, $dstReg: #Reg, $valTy: #ValueType)
{
  var $value'v0: #Value;
  var $o'v1: #Object;
  var $i'v2: #Int32;
  var $b'v3: #Bool;
  var $s'v4: #String;
  var $s'v5: #Symbol;
  var $b'v6: #BigInt;
  var out'0: #Value;
  var out'1: #Object;
  var out'2: #Int32;
  var out'3: #Bool;
  var out'4: #String;
  var out'5: #Symbol;
  var out'6: #BigInt;
  
  assert ($valTy != #ValueType^Variant~Double());
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  if ($valTy == #ValueType^Variant~Object()) {
    call out'1 := #Value~toObject($value'v0);
    $o'v1 := out'1;
    call #MASM~setObject($dstReg, $o'v1);
  } else if ($valTy == #ValueType^Variant~Int32()) {
    call out'2 := #Value~toInt32($value'v0);
    $i'v2 := out'2;
    call #MASM~setInt32($dstReg, $i'v2);
  } else if ($valTy == #ValueType^Variant~Bool()) {
    call out'3 := #Value~toBool($value'v0);
    $b'v3 := out'3;
    call #MASM~setBool($dstReg, $b'v3);
  } else if ($valTy == #ValueType^Variant~String()) {
    call out'4 := #Value~toString($value'v0);
    $s'v4 := out'4;
    call #MASM~setString($dstReg, $s'v4);
  } else if ($valTy == #ValueType^Variant~Symbol()) {
    call out'5 := #Value~toSymbol($value'v0);
    $s'v5 := out'5;
    call #MASM~setSymbol($dstReg, $s'v5);
  } else if ($valTy == #ValueType^Variant~BigInt()) {
    call out'6 := #Value~toBigInt($value'v0);
    $b'v6 := out'6;
    call #MASM~setBigInt($dstReg, $b'v6);
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~CastBoolToInt32($int32Reg: #Reg): #MASM^Op;

procedure #MASM~CastBoolToInt32($int32Reg: #Reg)
{
  var $bool'v0: #Bool;
  var out'0: #Bool;
  
  call out'0 := #MASM~getBool($int32Reg);
  $bool'v0 := out'0;
  if ($bool'v0) {
    call #MASM~setInt32($int32Reg, 1bv32);
  } else {
    call #MASM~setInt32($int32Reg, 0bv32);
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~ConvertInt32ValueToDouble($valueReg: #ValueReg): #MASM^Op;

procedure #MASM~ConvertInt32ValueToDouble($valueReg: #ValueReg)
{
  var $value'v0: #Value;
  var $int32'v1: #Int32;
  var $double'v2: #Double;
  var $tmp'v3: #Value;
  var out'0: #Value;
  var out'1: #Bool;
  var out'2: #Int32;
  var out'3: #Value;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isInt32($value'v0);
  if (!out'1) {
    call #MASM^step();
    return;
  }
  call out'2 := #Value~toInt32($value'v0);
  $int32'v1 := out'2;
  $double'v2 := #Int32^to#Double($int32'v1);
  call out'3 := #Value~fromDouble($double'v2);
  $tmp'v3 := out'3;
  call #MASM~setValue($valueReg, $tmp'v3);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestNumber($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestNumber($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsNumber'v1: #Bool;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var $tmp'v6: #Bool;
  var $tmp'v7: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  var out'2: #Bool;
  
  $tmp'v2 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v2 := ($condition == #Condition^Variant~NotEqual());
  }
  assert $tmp'v2;
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  $tmp'v5 := true;
  call out'1 := #Value~isInt32($value'v0);
  $tmp'v3 := out'1;
  if (!$tmp'v3) {
    call out'2 := #Value~isDouble($value'v0);
    $tmp'v4 := out'2;
    $tmp'v5 := $tmp'v4;
  }
  $valueIsNumber'v1 := $tmp'v5;
  $tmp'v6 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v6 := $valueIsNumber'v1;
  }
  $tmp'v7 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v7 := (!$valueIsNumber'v1);
  }
  if ($tmp'v6) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v7) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestDouble($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestDouble($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsDouble'v1: #Bool;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  $tmp'v2 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v2 := ($condition == #Condition^Variant~NotEqual());
  }
  assert $tmp'v2;
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isDouble($value'v0);
  $valueIsDouble'v1 := out'1;
  $tmp'v3 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v3 := $valueIsDouble'v1;
  }
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v4 := (!$valueIsDouble'v1);
  }
  if ($tmp'v3) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchObject($condition: #Condition, $lhsReg: #Reg, $rhsReg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchObject($condition: #Condition, $lhsReg: #Reg, $rhsReg: #Reg, $branch: EmitPath)
{
  var $lhs'v0: #Object;
  var $rhs'v1: #Object;
  var $isEqual'v2: #Bool;
  var out'0: #Object;
  var out'1: #Object;
  
  call out'0 := #MASM~getObject($lhsReg);
  $lhs'v0 := out'0;
  call out'1 := #MASM~getObject($rhsReg);
  $rhs'v1 := out'1;
  $isEqual'v2 := ($lhs'v0 == $rhs'v1);
  if ($condition == #Condition^Variant~Equal()) {
    if ($isEqual'v2) {
      call #MASM^goto($branch);
      return;
    }
  }
  if ($condition == #Condition^Variant~NotEqual()) {
    if (!$isEqual'v2) {
      call #MASM^goto($branch);
      return;
    }
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestInt32($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestInt32($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsInt32'v1: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isInt32($value'v0);
  $valueIsInt32'v1 := out'1;
  if ($condition == #Condition^Variant~Equal()) {
    if ($valueIsInt32'v1) {
      call #MASM^goto($branch);
      return;
    }
  } else if ($condition == #Condition^Variant~NotEqual()) {
    if (!$valueIsInt32'v1) {
      call #MASM^goto($branch);
      return;
    }
  } else {
    assert false;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestInt32Truthy($truthy: #Bool, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestInt32Truthy($truthy: #Bool, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $int32'v1: #Int32;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Value;
  var out'1: #Int32;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~toInt32($value'v0);
  $int32'v1 := out'1;
  $tmp'v4 := true;
  $tmp'v2 := false;
  if (!$truthy) {
    $tmp'v2 := ($int32'v1 == 0bv32);
  }
  if (!$tmp'v2) {
    $tmp'v3 := false;
    if ($truthy) {
      $tmp'v3 := ($int32'v1 != 0bv32);
    }
    $tmp'v4 := $tmp'v3;
  }
  if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestBoolean($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestBoolean($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsBool'v1: #Bool;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  $tmp'v2 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v2 := ($condition == #Condition^Variant~NotEqual());
  }
  assert $tmp'v2;
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isBool($value'v0);
  $valueIsBool'v1 := out'1;
  $tmp'v3 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v3 := $valueIsBool'v1;
  }
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v4 := (!$valueIsBool'v1);
  }
  if ($tmp'v3) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestString($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestString($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsString'v1: #Bool;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  $tmp'v2 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v2 := ($condition == #Condition^Variant~NotEqual());
  }
  assert $tmp'v2;
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isString($value'v0);
  $valueIsString'v1 := out'1;
  $tmp'v3 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v3 := $valueIsString'v1;
  }
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v4 := (!$valueIsString'v1);
  }
  if ($tmp'v3) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestSymbol($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestSymbol($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsSymbol'v1: #Bool;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  $tmp'v2 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v2 := ($condition == #Condition^Variant~NotEqual());
  }
  assert $tmp'v2;
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isSymbol($value'v0);
  $valueIsSymbol'v1 := out'1;
  $tmp'v3 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v3 := $valueIsSymbol'v1;
  }
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v4 := (!$valueIsSymbol'v1);
  }
  if ($tmp'v3) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestBigInt($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestBigInt($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsBigInt'v1: #Bool;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  $tmp'v2 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v2 := ($condition == #Condition^Variant~NotEqual());
  }
  assert $tmp'v2;
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isBigInt($value'v0);
  $valueIsBigInt'v1 := out'1;
  $tmp'v3 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v3 := $valueIsBigInt'v1;
  }
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v4 := (!$valueIsBigInt'v1);
  }
  if ($tmp'v3) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestObject($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestObject($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsObject'v1: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isObject($value'v0);
  $valueIsObject'v1 := out'1;
  if ($condition == #Condition^Variant~Equal()) {
    if ($valueIsObject'v1) {
      call #MASM^goto($branch);
      return;
    }
  }
  if ($condition == #Condition^Variant~NotEqual()) {
    if (!$valueIsObject'v1) {
      call #MASM^goto($branch);
      return;
    }
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestValue($cond: #Condition, $lhs: #BaseIndex, $rhs: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestValue($cond: #Condition, $lhs: #BaseIndex, $rhs: #ValueReg, $branch: EmitPath)
{
  var $rhsReg'v0: #Reg;
  var $lhsOp'v1: #Operand;
  var $tmp'v2: #Bool;
  var out'0: #Reg;
  
  $tmp'v2 := true;
  if (!($cond == #Condition^Variant~Equal())) {
    $tmp'v2 := ($cond == #Condition^Variant~NotEqual());
  }
  assert $tmp'v2;
  call out'0 := #ValueReg~scratchReg($rhs);
  $rhsReg'v0 := out'0;
  $lhsOp'v1 := #Operand~fromBaseIndex($lhs);
  if (#Operand~compareToReg($cond, $lhsOp'v1, $rhsReg'v0)) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestNullProto($reg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestNullProto($reg: #Reg, $branch: EmitPath)
{
  var $data'v0: #RegData;
  var $proto'v1: #TaggedProto;
  var out'0: #RegData;
  var out'1: #TaggedProto;
  var out'2: #Bool;
  
  call out'0 := #MASM~getData($reg);
  $data'v0 := out'0;
  call out'1 := #RegData~toTaggedProto($data'v0);
  $proto'v1 := out'1;
  call out'2 := #TaggedProto~isNull($proto'v1);
  if (out'2) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestLazyProto($reg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestLazyProto($reg: #Reg, $branch: EmitPath)
{
  var $data'v0: #RegData;
  var $proto'v1: #TaggedProto;
  var out'0: #RegData;
  var out'1: #TaggedProto;
  var out'2: #Bool;
  
  call out'0 := #MASM~getData($reg);
  $data'v0 := out'0;
  call out'1 := #RegData~toTaggedProto($data'v0);
  $proto'v1 := out'1;
  call out'2 := #TaggedProto~isLazy($proto'v1);
  if (out'2) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestNull($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestNull($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsNull'v1: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isNull($value'v0);
  $valueIsNull'v1 := out'1;
  if ($condition == #Condition^Variant~Equal()) {
    if ($valueIsNull'v1) {
      call #MASM^goto($branch);
      return;
    }
  }
  if ($condition == #Condition^Variant~NotEqual()) {
    if (!$valueIsNull'v1) {
      call #MASM^goto($branch);
      return;
    }
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestUndefined($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestUndefined($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsUndefined'v1: #Bool;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  $tmp'v2 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v2 := ($condition == #Condition^Variant~NotEqual());
  }
  assert $tmp'v2;
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isUndefined($value'v0);
  $valueIsUndefined'v1 := out'1;
  $tmp'v3 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v3 := $valueIsUndefined'v1;
  }
  if ($tmp'v3) {
    call #MASM^goto($branch);
    return;
  }
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v4 := (!$valueIsUndefined'v1);
  }
  if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestMagic($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestMagic($condition: #Condition, $valueReg: #ValueReg, $branch: EmitPath)
{
  var $value'v0: #Value;
  var $valueIsMagic'v1: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isMagic($value'v0);
  $valueIsMagic'v1 := out'1;
  if ($condition == #Condition^Variant~Equal()) {
    if ($valueIsMagic'v1) {
      call #MASM^goto($branch);
      return;
    }
  }
  if ($condition == #Condition^Variant~NotEqual()) {
    if (!$valueIsMagic'v1) {
      call #MASM^goto($branch);
      return;
    }
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestObjIsFunction($condition: #Condition, $objectReg: #Reg, $scratchReg: #Reg, $spectreRegToZero: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestObjIsFunction($condition: #Condition, $objectReg: #Reg, $scratchReg: #Reg, $spectreRegToZero: #Reg, $branch: EmitPath)
{
  var $object'v0: #Object;
  var $shape'v1: #Shape;
  var $class'v2: #Class;
  var $isClass'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Class;
  var $tmp'v6: #Bool;
  var $tmp'v7: #Class;
  var $tmp'v8: #Bool;
  var $tmp'v9: #Bool;
  var $tmp'v10: #Bool;
  var $tmp'v11: #Bool;
  var out'0: #Object;
  var out'1: #Shape;
  var out'2: #Class;
  var out'3: #Class;
  var out'4: #Class;
  
  $tmp'v4 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v4 := ($condition == #Condition^Variant~NotEqual());
  }
  assert $tmp'v4;
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~shapeOf($object'v0);
  $shape'v1 := out'1;
  call out'2 := #Shape~classOf($shape'v1);
  $class'v2 := out'2;
  $tmp'v9 := true;
  call out'3 := #Class~functionClass();
  $tmp'v5 := out'3;
  $tmp'v6 := ($class'v2 == $tmp'v5);
  if (!$tmp'v6) {
    call out'4 := #Class~extendedFunctionClass();
    $tmp'v7 := out'4;
    $tmp'v8 := ($class'v2 == $tmp'v7);
    $tmp'v9 := $tmp'v8;
  }
  $isClass'v3 := $tmp'v9;
  $tmp'v10 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v10 := $isClass'v3;
  }
  if ($tmp'v10) {
    call #MASM^goto($branch);
    return;
  }
  $tmp'v11 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v11 := (!$isClass'v3);
  }
  if ($tmp'v11) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM~setInt32($scratchReg, 0bv32);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestObjIsFunctionNoSpectreMitigations($condition: #Condition, $objectReg: #Reg, $scratchReg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestObjIsFunctionNoSpectreMitigations($condition: #Condition, $objectReg: #Reg, $scratchReg: #Reg, $branch: EmitPath)
{
  var $object'v0: #Object;
  var $shape'v1: #Shape;
  var $class'v2: #Class;
  var $isClass'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Class;
  var $tmp'v6: #Bool;
  var $tmp'v7: #Class;
  var $tmp'v8: #Bool;
  var $tmp'v9: #Bool;
  var $tmp'v10: #Bool;
  var $tmp'v11: #Bool;
  var out'0: #Object;
  var out'1: #Shape;
  var out'2: #Class;
  var out'3: #Class;
  var out'4: #Class;
  
  $tmp'v4 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v4 := ($condition == #Condition^Variant~NotEqual());
  }
  assert $tmp'v4;
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~shapeOf($object'v0);
  $shape'v1 := out'1;
  call out'2 := #Shape~classOf($shape'v1);
  $class'v2 := out'2;
  $tmp'v9 := true;
  call out'3 := #Class~functionClass();
  $tmp'v5 := out'3;
  $tmp'v6 := ($class'v2 == $tmp'v5);
  if (!$tmp'v6) {
    call out'4 := #Class~extendedFunctionClass();
    $tmp'v7 := out'4;
    $tmp'v8 := ($class'v2 == $tmp'v7);
    $tmp'v9 := $tmp'v8;
  }
  $isClass'v3 := $tmp'v9;
  $tmp'v10 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v10 := $isClass'v3;
  }
  $tmp'v11 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v11 := (!$isClass'v3);
  }
  if ($tmp'v10) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v11) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestObjectShape($condition: #Condition, $objectReg: #Reg, $shape: #Shape, $scratchReg: #Reg, $spectreRegToZero: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestObjectShape($condition: #Condition, $objectReg: #Reg, $shape: #Shape, $scratchReg: #Reg, $spectreRegToZero: #Reg, $branch: EmitPath)
{
  var $object'v0: #Object;
  var $objectHasShape'v1: #Bool;
  var $tmp'v2: #Shape;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Object;
  var out'1: #Shape;
  
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~shapeOf($object'v0);
  $tmp'v2 := out'1;
  $objectHasShape'v1 := ($tmp'v2 == $shape);
  call #MASM~setInt32($scratchReg, 0bv32);
  $tmp'v3 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v3 := $objectHasShape'v1;
  }
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v4 := (!$objectHasShape'v1);
  }
  if ($tmp'v3) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestObjectShapeNoSpectreMitigations($condition: #Condition, $objectReg: #Reg, $shape: #Shape, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestObjectShapeNoSpectreMitigations($condition: #Condition, $objectReg: #Reg, $shape: #Shape, $branch: EmitPath)
{
  var $object'v0: #Object;
  var $objectHasShape'v1: #Bool;
  var $tmp'v2: #Shape;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Object;
  var out'1: #Shape;
  
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~shapeOf($object'v0);
  $tmp'v2 := out'1;
  $objectHasShape'v1 := ($tmp'v2 == $shape);
  $tmp'v3 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v3 := $objectHasShape'v1;
  }
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v4 := (!$objectHasShape'v1);
  }
  if ($tmp'v3) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestObjectClass($condition: #Condition, $objectReg: #Reg, $class: #Class, $scratchReg: #Reg, $spectreRegToZero: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestObjectClass($condition: #Condition, $objectReg: #Reg, $class: #Class, $scratchReg: #Reg, $spectreRegToZero: #Reg, $branch: EmitPath)
{
  var $object'v0: #Object;
  var $shape'v1: #Shape;
  var $objectHasClass'v2: #Bool;
  var $tmp'v3: #Class;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var out'0: #Object;
  var out'1: #Shape;
  var out'2: #Class;
  
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~shapeOf($object'v0);
  $shape'v1 := out'1;
  call out'2 := #Shape~classOf($shape'v1);
  $tmp'v3 := out'2;
  $objectHasClass'v2 := ($tmp'v3 == $class);
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v4 := $objectHasClass'v2;
  }
  $tmp'v5 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v5 := (!$objectHasClass'v2);
  }
  if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v5) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM~setInt32($scratchReg, 0bv32);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestObjectClassNoSpectreMitigations($condition: #Condition, $objectReg: #Reg, $class: #Class, $scratchReg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestObjectClassNoSpectreMitigations($condition: #Condition, $objectReg: #Reg, $class: #Class, $scratchReg: #Reg, $branch: EmitPath)
{
  var $object'v0: #Object;
  var $shape'v1: #Shape;
  var $objectHasClass'v2: #Bool;
  var $tmp'v3: #Class;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var out'0: #Object;
  var out'1: #Shape;
  var out'2: #Class;
  
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~shapeOf($object'v0);
  $shape'v1 := out'1;
  call out'2 := #Shape~classOf($shape'v1);
  $tmp'v3 := out'2;
  $objectHasClass'v2 := ($tmp'v3 == $class);
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v4 := $objectHasClass'v2;
  }
  $tmp'v5 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v5 := (!$objectHasClass'v2);
  }
  if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v5) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchIfNonNativeObj($objectReg: #Reg, $scratchReg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchIfNonNativeObj($objectReg: #Reg, $scratchReg: #Reg, $branch: EmitPath)
{
  var $object'v0: #Object;
  var $shape'v1: #Shape;
  var $class'v2: #Class;
  var $objectIsNative'v3: #Bool;
  var out'0: #Object;
  var out'1: #Shape;
  var out'2: #Class;
  
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~shapeOf($object'v0);
  $shape'v1 := out'1;
  call out'2 := #Shape~classOf($shape'v1);
  $class'v2 := out'2;
  $objectIsNative'v3 := #Class~isNativeObject($class'v2);
  if (!$objectIsNative'v3) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestObjectIsProxy($proxy: #Bool, $objectReg: #Reg, $scratchReg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTestObjectIsProxy($proxy: #Bool, $objectReg: #Reg, $scratchReg: #Reg, $branch: EmitPath)
{
  var $object'v0: #Object;
  var $shape'v1: #Shape;
  var $class'v2: #Class;
  var $objectIsProxy'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var out'0: #Object;
  var out'1: #Shape;
  var out'2: #Class;
  
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~shapeOf($object'v0);
  $shape'v1 := out'1;
  call out'2 := #Shape~classOf($shape'v1);
  $class'v2 := out'2;
  $objectIsProxy'v3 := #Class~isProxyObject($class'v2);
  $tmp'v4 := false;
  if (!$proxy) {
    $tmp'v4 := (!$objectIsProxy'v3);
  }
  $tmp'v5 := false;
  if ($proxy) {
    $tmp'v5 := $objectIsProxy'v3;
  }
  if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v5) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Branch32Imm($condition: #Condition, $lhsReg: #Reg, $rhsInt32: #Int32, $branch: EmitPath): #MASM^Op;

procedure #MASM~Branch32Imm($condition: #Condition, $lhsReg: #Reg, $rhsInt32: #Int32, $branch: EmitPath)
{
  var $lhsInt32'v0: #Int32;
  var $tmp'v1: #Bool;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var out'0: #Int32;
  
  $tmp'v2 := true;
  $tmp'v1 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v1 := ($condition == #Condition^Variant~NotEqual());
  }
  if (!$tmp'v1) {
    $tmp'v2 := ($condition == #Condition^Variant~LessThan());
  }
  assert $tmp'v2;
  call out'0 := #MASM~getInt32($lhsReg);
  $lhsInt32'v0 := out'0;
  $tmp'v3 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v3 := ($lhsInt32'v0 == $rhsInt32);
  }
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v4 := ($lhsInt32'v0 != $rhsInt32);
  }
  $tmp'v5 := false;
  if ($condition == #Condition^Variant~LessThan()) {
    $tmp'v5 := #Int32^lt($lhsInt32'v0, $rhsInt32);
  }
  if ($tmp'v3) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v5) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Branch32AddressImm32($condition: #Condition, $address: #Address, $rhsInt32: #Int32, $branch: EmitPath): #MASM^Op;

procedure #MASM~Branch32AddressImm32($condition: #Condition, $address: #Address, $rhsInt32: #Int32, $branch: EmitPath)
{
  var $baseData'v0: #RegData;
  var $data'v1: #RegData;
  var $lhsInt32'v2: #Int32;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Value;
  var $tmp'v6: #Bool;
  var $tmp'v7: #Bool;
  var $tmp'v8: #Bool;
  var out'0: #RegData;
  var out'1: #RegData;
  var out'2: #Value;
  var out'3: #Int32;
  
  $tmp'v4 := true;
  $tmp'v3 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v3 := ($condition == #Condition^Variant~NotEqual());
  }
  if (!$tmp'v3) {
    $tmp'v4 := ($condition == #Condition^Variant~LessThan());
  }
  assert $tmp'v4;
  call out'0 := #MASM~getData(#Address^field~base($address));
  $baseData'v0 := out'0;
  call out'1 := #RegData~readData($baseData'v0, #Int32^to#Int64(#Address^field~offset($address)));
  $data'v1 := out'1;
  call out'2 := #RegData~toUnboxedValue($data'v1);
  $tmp'v5 := out'2;
  call out'3 := #Value~toInt32($tmp'v5);
  $lhsInt32'v2 := out'3;
  $tmp'v6 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v6 := ($lhsInt32'v2 == $rhsInt32);
  }
  $tmp'v7 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v7 := ($lhsInt32'v2 != $rhsInt32);
  }
  $tmp'v8 := false;
  if ($condition == #Condition^Variant~LessThan()) {
    $tmp'v8 := #Int32^lt($lhsInt32'v2, $rhsInt32);
  }
  if ($tmp'v6) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v7) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v8) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTest32($condition: #Condition, $lhsReg: #Reg, $rhsReg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTest32($condition: #Condition, $lhsReg: #Reg, $rhsReg: #Reg, $branch: EmitPath)
{
  var $lhsInt32'v0: #Int32;
  var $rhsInt32'v1: #Int32;
  var $result'v2: #Int32;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($lhsReg);
  $lhsInt32'v0 := out'0;
  call out'1 := #MASM~getInt32($rhsReg);
  $rhsInt32'v1 := out'1;
  $result'v2 := #Int32^bitAnd($lhsInt32'v0, $rhsInt32'v1);
  $tmp'v5 := true;
  $tmp'v4 := true;
  $tmp'v3 := true;
  if (!($condition == #Condition^Variant~Zero())) {
    $tmp'v3 := ($condition == #Condition^Variant~NonZero());
  }
  if (!$tmp'v3) {
    $tmp'v4 := ($condition == #Condition^Variant~Signed());
  }
  if (!$tmp'v4) {
    $tmp'v5 := ($condition == #Condition^Variant~NotSigned());
  }
  assert $tmp'v5;
  if ($condition == #Condition^Variant~Zero()) {
    if ($result'v2 == 0bv32) {
      call #MASM^goto($branch);
      return;
    }
  } else if ($condition == #Condition^Variant~NonZero()) {
    if ($result'v2 != 0bv32) {
      call #MASM^goto($branch);
      return;
    }
  } else if ($condition == #Condition^Variant~Signed()) {
    if (#Int32^lt($result'v2, 0bv32)) {
      call #MASM^goto($branch);
      return;
    }
  } else if ($condition == #Condition^Variant~NotSigned()) {
    if (#Int32^gt($result'v2, 0bv32)) {
      call #MASM^goto($branch);
      return;
    }
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTest32Imm($condition: #Condition, $lhsReg: #Reg, $rhsInt32: #Int32, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchTest32Imm($condition: #Condition, $lhsReg: #Reg, $rhsInt32: #Int32, $branch: EmitPath)
{
  var $lhsInt32'v0: #Int32;
  var $result'v1: #Int32;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Int32;
  
  call out'0 := #MASM~getInt32($lhsReg);
  $lhsInt32'v0 := out'0;
  $result'v1 := #Int32^bitAnd($lhsInt32'v0, $rhsInt32);
  $tmp'v4 := true;
  $tmp'v3 := true;
  $tmp'v2 := true;
  if (!($condition == #Condition^Variant~Zero())) {
    $tmp'v2 := ($condition == #Condition^Variant~NonZero());
  }
  if (!$tmp'v2) {
    $tmp'v3 := ($condition == #Condition^Variant~Signed());
  }
  if (!$tmp'v3) {
    $tmp'v4 := ($condition == #Condition^Variant~NotSigned());
  }
  assert $tmp'v4;
  if ($condition == #Condition^Variant~Zero()) {
    if ($result'v1 == 0bv32) {
      call #MASM^goto($branch);
      return;
    }
  } else if ($condition == #Condition^Variant~NonZero()) {
    if ($result'v1 != 0bv32) {
      call #MASM^goto($branch);
      return;
    }
  } else if ($condition == #Condition^Variant~Signed()) {
    if (#Int32^lt($result'v1, 0bv32)) {
      call #MASM^goto($branch);
      return;
    }
  } else if ($condition == #Condition^Variant~NotSigned()) {
    if (#Int32^gt($result'v1, 0bv32)) {
      call #MASM^goto($branch);
      return;
    }
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchAdd32($condition: #Condition, $srcReg: #Reg, $dstReg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchAdd32($condition: #Condition, $srcReg: #Reg, $dstReg: #Reg, $branch: EmitPath)
{
  var $lhsInt32'v0: #Int32;
  var $rhsInt32'v1: #Int32;
  var $result'v2: #Int32;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var $tmp'v6: #Bool;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($srcReg);
  $lhsInt32'v0 := out'0;
  call out'1 := #MASM~getInt32($dstReg);
  $rhsInt32'v1 := out'1;
  $result'v2 := #Int32^add($lhsInt32'v0, $rhsInt32'v1);
  assert ($condition == #Condition^Variant~Overflow());
  $tmp'v4 := false;
  $tmp'v3 := false;
  if (#Int32^gt($lhsInt32'v0, 0bv32)) {
    $tmp'v3 := #Int32^gt($rhsInt32'v1, 0bv32);
  }
  if ($tmp'v3) {
    $tmp'v4 := #Int32^lt($result'v2, 0bv32);
  }
  if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  }
  $tmp'v6 := false;
  $tmp'v5 := false;
  if (#Int32^lt($lhsInt32'v0, 0bv32)) {
    $tmp'v5 := #Int32^lt($rhsInt32'v1, 0bv32);
  }
  if ($tmp'v5) {
    $tmp'v6 := #Int32^gt($result'v2, 0bv32);
  }
  if ($tmp'v6) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM~setInt32($dstReg, $result'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchAdd32Imm($condition: #Condition, $lhsInt32: #Int32, $dstReg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchAdd32Imm($condition: #Condition, $lhsInt32: #Int32, $dstReg: #Reg, $branch: EmitPath)
{
  var $rhsInt32'v0: #Int32;
  var $result'v1: #Int32;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var out'0: #Int32;
  
  call out'0 := #MASM~getInt32($dstReg);
  $rhsInt32'v0 := out'0;
  $result'v1 := #Int32^add($lhsInt32, $rhsInt32'v0);
  assert ($condition == #Condition^Variant~Overflow());
  $tmp'v3 := false;
  $tmp'v2 := false;
  if (#Int32^gt($lhsInt32, 0bv32)) {
    $tmp'v2 := #Int32^gt($rhsInt32'v0, 0bv32);
  }
  if ($tmp'v2) {
    $tmp'v3 := #Int32^lt($result'v1, 0bv32);
  }
  if ($tmp'v3) {
    call #MASM^goto($branch);
    return;
  }
  $tmp'v5 := false;
  $tmp'v4 := false;
  if (#Int32^lt($lhsInt32, 0bv32)) {
    $tmp'v4 := #Int32^lt($rhsInt32'v0, 0bv32);
  }
  if ($tmp'v4) {
    $tmp'v5 := #Int32^gt($result'v1, 0bv32);
  }
  if ($tmp'v5) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM~setInt32($dstReg, $result'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchSub32($condition: #Condition, $srcReg: #Reg, $dstReg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchSub32($condition: #Condition, $srcReg: #Reg, $dstReg: #Reg, $branch: EmitPath)
{
  var $lhsInt32'v0: #Int32;
  var $rhsInt32'v1: #Int32;
  var $result'v2: #Int32;
  var $rhsInt32Neg'v3: #Int32;
  var $minInt32'v4: #Int32;
  var $tmp'v5: #Bool;
  var $tmp'v6: #Bool;
  var $tmp'v7: #Bool;
  var $tmp'v8: #Bool;
  var $tmp'v9: #Bool;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($dstReg);
  $lhsInt32'v0 := out'0;
  call out'1 := #MASM~getInt32($srcReg);
  $rhsInt32'v1 := out'1;
  $result'v2 := #Int32^sub($lhsInt32'v0, $rhsInt32'v1);
  assert ($condition == #Condition^Variant~Overflow());
  $rhsInt32Neg'v3 := #Int32^negate($rhsInt32'v1);
  $minInt32'v4 := #Int32^shl(1bv32, 31bv32);
  $tmp'v5 := false;
  if ($rhsInt32'v1 == $minInt32'v4) {
    $tmp'v5 := #Int32^gte($lhsInt32'v0, 0bv32);
  }
  $tmp'v7 := false;
  $tmp'v6 := false;
  if (#Int32^gt($lhsInt32'v0, 0bv32)) {
    $tmp'v6 := #Int32^lt($rhsInt32'v1, 0bv32);
  }
  if ($tmp'v6) {
    $tmp'v7 := #Int32^lt($result'v2, 0bv32);
  }
  if ($tmp'v5) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v7) {
    call #MASM^goto($branch);
    return;
  }
  $tmp'v9 := false;
  $tmp'v8 := false;
  if (#Int32^lt($lhsInt32'v0, 0bv32)) {
    $tmp'v8 := #Int32^gt($rhsInt32'v1, 0bv32);
  }
  if ($tmp'v8) {
    $tmp'v9 := #Int32^gt($result'v2, 0bv32);
  }
  if ($tmp'v9) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM~setInt32($dstReg, $result'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchSub32Imm($condition: #Condition, $rhsInt32: #Int32, $dstReg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchSub32Imm($condition: #Condition, $rhsInt32: #Int32, $dstReg: #Reg, $branch: EmitPath)
{
  var $lhsInt32'v0: #Int32;
  var $result'v1: #Int32;
  var $rhsInt32Neg'v2: #Int32;
  var $minInt32'v3: #Int32;
  var $tmp'v4: #Bool;
  var $tmp'v5: #Bool;
  var $tmp'v6: #Bool;
  var $tmp'v7: #Bool;
  var $tmp'v8: #Bool;
  var out'0: #Int32;
  
  call out'0 := #MASM~getInt32($dstReg);
  $lhsInt32'v0 := out'0;
  $result'v1 := #Int32^sub($lhsInt32'v0, $rhsInt32);
  assert ($condition == #Condition^Variant~Overflow());
  $rhsInt32Neg'v2 := #Int32^negate($rhsInt32);
  $minInt32'v3 := #Int32^shl(1bv32, 31bv32);
  $tmp'v4 := false;
  if ($rhsInt32 == $minInt32'v3) {
    $tmp'v4 := #Int32^gte($lhsInt32'v0, 0bv32);
  }
  $tmp'v6 := false;
  $tmp'v5 := false;
  if (#Int32^gt($lhsInt32'v0, 0bv32)) {
    $tmp'v5 := #Int32^lt($rhsInt32, 0bv32);
  }
  if ($tmp'v5) {
    $tmp'v6 := #Int32^lt($result'v1, 0bv32);
  }
  if ($tmp'v4) {
    call #MASM^goto($branch);
    return;
  } else if ($tmp'v6) {
    call #MASM^goto($branch);
    return;
  }
  $tmp'v8 := false;
  $tmp'v7 := false;
  if (#Int32^lt($lhsInt32'v0, 0bv32)) {
    $tmp'v7 := #Int32^gt($rhsInt32, 0bv32);
  }
  if ($tmp'v7) {
    $tmp'v8 := #Int32^gt($result'v1, 0bv32);
  }
  if ($tmp'v8) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM~setInt32($dstReg, $result'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchMul32($condition: #Condition, $srcReg: #Reg, $dstReg: #Reg, $branch: EmitPath): #MASM^Op;

procedure #MASM~BranchMul32($condition: #Condition, $srcReg: #Reg, $dstReg: #Reg, $branch: EmitPath)
{
  var $lhsInt32'v0: #Int32;
  var $rhsInt32'v1: #Int32;
  var $result'v2: #Int32;
  var $resultWide'v3: #Int64;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($dstReg);
  $lhsInt32'v0 := out'0;
  call out'1 := #MASM~getInt32($srcReg);
  $rhsInt32'v1 := out'1;
  $result'v2 := #Int32^mul($lhsInt32'v0, $rhsInt32'v1);
  assert ($condition == #Condition^Variant~Overflow());
  $resultWide'v3 := #Int64^mul(#Int32^to#Int64($lhsInt32'v0), #Int32^to#Int64($rhsInt32'v1));
  if (#Int32^to#Int64($result'v2) != $resultWide'v3) {
    call #MASM^goto($branch);
    return;
  }
  call #MASM~setInt32($dstReg, $result'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~FlexibleDivMod32($rhsReg: #Reg, $lhsOutputReg: #Reg, $remReg: #Reg): #MASM^Op;

procedure #MASM~FlexibleDivMod32($rhsReg: #Reg, $lhsOutputReg: #Reg, $remReg: #Reg)
{
  var $lhsInt32'v0: #Int32;
  var $rhsInt32'v1: #Int32;
  var $quotient'v2: #Int32;
  var $remainder'v3: #Int32;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($lhsOutputReg);
  $lhsInt32'v0 := out'0;
  call out'1 := #MASM~getInt32($rhsReg);
  $rhsInt32'v1 := out'1;
  $quotient'v2 := #Int32^div($lhsInt32'v0, $rhsInt32'v1);
  $remainder'v3 := #Int32^mod($lhsInt32'v0, $rhsInt32'v1);
  call #MASM~setInt32($lhsOutputReg, $quotient'v2);
  call #MASM~setInt32($remReg, $remainder'v3);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~FlexibleRemainder32($rhsReg: #Reg, $lhsOutputReg: #Reg): #MASM^Op;

procedure #MASM~FlexibleRemainder32($rhsReg: #Reg, $lhsOutputReg: #Reg)
{
  var $lhsInt32'v0: #Int32;
  var $rhsInt32'v1: #Int32;
  var $remainder'v2: #Int32;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($lhsOutputReg);
  $lhsInt32'v0 := out'0;
  call out'1 := #MASM~getInt32($rhsReg);
  $rhsInt32'v1 := out'1;
  $remainder'v2 := #Int32^mod($lhsInt32'v0, $rhsInt32'v1);
  call #MASM~setInt32($lhsOutputReg, $remainder'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Neg32($valueReg: #Reg): #MASM^Op;

procedure #MASM~Neg32($valueReg: #Reg)
{
  var $valueInt32'v0: #Int32;
  var $result'v1: #Int32;
  var out'0: #Int32;
  
  call out'0 := #MASM~getInt32($valueReg);
  $valueInt32'v0 := out'0;
  $result'v1 := #Int32^negate($valueInt32'v0);
  call #MASM~setInt32($valueReg, $result'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Not32($valueReg: #Reg): #MASM^Op;

procedure #MASM~Not32($valueReg: #Reg)
{
  var $valueInt32'v0: #Int32;
  var $result'v1: #Int32;
  var out'0: #Int32;
  
  call out'0 := #MASM~getInt32($valueReg);
  $valueInt32'v0 := out'0;
  $result'v1 := #Int32^bitNot($valueInt32'v0);
  call #MASM~setInt32($valueReg, $result'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Or32($srcReg: #Reg, $dstReg: #Reg): #MASM^Op;

procedure #MASM~Or32($srcReg: #Reg, $dstReg: #Reg)
{
  var $lhsInt32'v0: #Int32;
  var $rhsInt32'v1: #Int32;
  var $result'v2: #Int32;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($srcReg);
  $lhsInt32'v0 := out'0;
  call out'1 := #MASM~getInt32($dstReg);
  $rhsInt32'v1 := out'1;
  $result'v2 := #Int32^bitOr($lhsInt32'v0, $rhsInt32'v1);
  call #MASM~setInt32($dstReg, $result'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~Xor32($srcReg: #Reg, $dstReg: #Reg): #MASM^Op;

procedure #MASM~Xor32($srcReg: #Reg, $dstReg: #Reg)
{
  var $lhsInt32'v0: #Int32;
  var $rhsInt32'v1: #Int32;
  var $result'v2: #Int32;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($srcReg);
  $lhsInt32'v0 := out'0;
  call out'1 := #MASM~getInt32($dstReg);
  $rhsInt32'v1 := out'1;
  $result'v2 := #Int32^xor($lhsInt32'v0, $rhsInt32'v1);
  call #MASM~setInt32($dstReg, $result'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~And32($srcReg: #Reg, $dstReg: #Reg): #MASM^Op;

procedure #MASM~And32($srcReg: #Reg, $dstReg: #Reg)
{
  var $lhsInt32'v0: #Int32;
  var $rhsInt32'v1: #Int32;
  var $result'v2: #Int32;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($srcReg);
  $lhsInt32'v0 := out'0;
  call out'1 := #MASM~getInt32($dstReg);
  $rhsInt32'v1 := out'1;
  $result'v2 := #Int32^bitAnd($lhsInt32'v0, $rhsInt32'v1);
  call #MASM~setInt32($dstReg, $result'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~FlexibleLshift32($shiftReg: #Reg, $srcDestReg: #Reg): #MASM^Op;

procedure #MASM~FlexibleLshift32($shiftReg: #Reg, $srcDestReg: #Reg)
{
  var $shiftInt32'v0: #Int32;
  var $srcInt32'v1: #Int32;
  var $result'v2: #Int32;
  var $tmp'v3: #Int32;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($shiftReg);
  $tmp'v3 := out'0;
  $shiftInt32'v0 := #Int32^bitAnd($tmp'v3, 31bv32);
  call out'1 := #MASM~getInt32($srcDestReg);
  $srcInt32'v1 := out'1;
  $result'v2 := #Int32^shl($srcInt32'v1, $shiftInt32'v0);
  call #MASM~setInt32($srcDestReg, $result'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~FlexibleRshift32Arithmetic($shiftReg: #Reg, $srcDestReg: #Reg): #MASM^Op;

procedure #MASM~FlexibleRshift32Arithmetic($shiftReg: #Reg, $srcDestReg: #Reg)
{
  var $shiftInt32'v0: #Int32;
  var $srcInt32'v1: #Int32;
  var $result'v2: #Int32;
  var $tmp'v3: #Int32;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($shiftReg);
  $tmp'v3 := out'0;
  $shiftInt32'v0 := #Int32^bitAnd($tmp'v3, 31bv32);
  call out'1 := #MASM~getInt32($srcDestReg);
  $srcInt32'v1 := out'1;
  $result'v2 := #Int32^shr($srcInt32'v1, $shiftInt32'v0);
  call #MASM~setInt32($srcDestReg, $result'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~FlexibleRshift32($shiftReg: #Reg, $srcDestReg: #Reg): #MASM^Op;

procedure #MASM~FlexibleRshift32($shiftReg: #Reg, $srcDestReg: #Reg)
{
  var $shiftInt32'v0: #Int32;
  var $srcInt32'v1: #Int32;
  var $result'v2: #Int32;
  var $tmp'v3: #Int32;
  var out'0: #Int32;
  var out'1: #Int32;
  
  call out'0 := #MASM~getInt32($shiftReg);
  $tmp'v3 := out'0;
  $shiftInt32'v0 := #Int32^bitAnd($tmp'v3, 31bv32);
  call out'1 := #MASM~getInt32($srcDestReg);
  $srcInt32'v1 := out'1;
  $result'v2 := #UInt32^to#Int32(#UInt32^shr(#Int32^to#UInt32($srcInt32'v1), #Int32^to#UInt32($shiftInt32'v0)));
  call #MASM~setInt32($srcDestReg, $result'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~TestObjectSet($condition: #Condition, $valueReg: #ValueReg, $dstReg: #Reg): #MASM^Op;

procedure #MASM~TestObjectSet($condition: #Condition, $valueReg: #ValueReg, $dstReg: #Reg)
{
  var $value'v0: #Value;
  var $isObject'v1: #Bool;
  var $tmp'v2: #Bool;
  var $tmp'v3: #Bool;
  var $tmp'v4: #Bool;
  var out'0: #Value;
  var out'1: #Bool;
  
  $tmp'v2 := true;
  if (!($condition == #Condition^Variant~Equal())) {
    $tmp'v2 := ($condition == #Condition^Variant~NotEqual());
  }
  assert $tmp'v2;
  call out'0 := #MASM~getValue($valueReg);
  $value'v0 := out'0;
  call out'1 := #Value~isObject($value'v0);
  $isObject'v1 := out'1;
  call #MASM~setBool($dstReg, false);
  $tmp'v3 := false;
  if ($condition == #Condition^Variant~Equal()) {
    $tmp'v3 := $isObject'v1;
  }
  $tmp'v4 := false;
  if ($condition == #Condition^Variant~NotEqual()) {
    $tmp'v4 := (!$isObject'v1);
  }
  if ($tmp'v3) {
    call #MASM~setBool($dstReg, true);
  } else if ($tmp'v4) {
    call #MASM~setBool($dstReg, true);
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~LoadObjectFixedSlot($objectReg: #Reg, $slot: #UInt32, $outputReg: #ValueReg): #MASM^Op;

procedure #MASM~LoadObjectFixedSlot($objectReg: #Reg, $slot: #UInt32, $outputReg: #ValueReg)
{
  var $object'v0: #Object;
  var $tmp'v1: #Value;
  var out'0: #Object;
  var out'1: #Value;
  
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~getFixedSlot($object'v0, $slot);
  $tmp'v1 := out'1;
  call #MASM~setValue($outputReg, $tmp'v1);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~LoadArgumentsObjectLength($objectReg: #Reg, $outputReg: #Reg, $failure: EmitPath): #MASM^Op;

procedure #MASM~LoadArgumentsObjectLength($objectReg: #Reg, $outputReg: #Reg, $failure: EmitPath)
{
  var $obj'v0: #Object;
  var $argObj'v1: #ArgumentsObject;
  var $length'v2: #Int32;
  var out'0: #Object;
  var out'1: #ArgumentsObject;
  
  call out'0 := #MASM~getObject($objectReg);
  $obj'v0 := out'0;
  call out'1 := #Object~toArgumentsObject($obj'v0);
  $argObj'v1 := out'1;
  if (#ArgumentsObject~hasOverriddenLength($argObj'v1)) {
    call #MASM^goto($failure);
    return;
  }
  $length'v2 := #ArgumentsObject~getInitialLength($argObj'v1);
  call #MASM~setInt32($outputReg, $length'v2);
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~LoadObjectProto($objectReg: #Reg, $protoReg: #Reg): #MASM^Op;

procedure #MASM~LoadObjectProto($objectReg: #Reg, $protoReg: #Reg)
{
  var $object'v0: #Object;
  var $shape'v1: #Shape;
  var $baseShape'v2: #BaseShape;
  var $proto'v3: #TaggedProto;
  var $data'v4: #RegData;
  var out'0: #Object;
  var out'1: #Shape;
  var out'2: #TaggedProto;
  var out'3: #RegData;
  
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~shapeOf($object'v0);
  $shape'v1 := out'1;
  $baseShape'v2 := #Shape~baseShapeOf($shape'v1);
  call out'2 := #BaseShape~protoOf($baseShape'v2);
  $proto'v3 := out'2;
  call out'3 := #RegData~fromTaggedProto($proto'v3);
  $data'v4 := out'3;
  call #MASM~setData($protoReg, $data'v4);
  call #MASM^step();
  return;
}

procedure #CacheIR~ReturnFromIC(emitPath: EmitPath)
{
  return;
}

procedure #CacheIR~GuardIsNumber($valueId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: EmitPath;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestNumber(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~GuardIsNullOrUndefined($valueId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: EmitPath;
  var $success'l1: EmitPath;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  $success'l1 := ConsEmitPath(emitPath, 1);
  call #MASM^emit(#MASM^Op~BranchTestNull(#Condition^Variant~Equal(), $valueReg'v0, $success'l1), ConsEmitPath(emitPath, 1));
  call #MASM^emit(#MASM^Op~BranchTestUndefined(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 2));
  call #MASM^bind($success'l1);
  return;
}

procedure #CacheIR~GuardIsNull($valueId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: EmitPath;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestNull(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~GuardIsUndefined($valueId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: EmitPath;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestUndefined(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~GuardIsNativeObject($objectId: #ObjectId, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $scratchReg'v1: #Reg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  
  call out'0 := #CacheIR~useObjectId($objectId, ConsEmitPath(emitPath, 0));
  $objectReg'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $scratchReg'v1 := out'1;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchIfNonNativeObj($objectReg'v0, $scratchReg'v1, $failure'l0), ConsEmitPath(emitPath, 1));
  call #CacheIR~releaseReg($scratchReg'v1);
  return;
}

procedure #CacheIR~GuardIsProxy($objectId: #ObjectId, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $scratchReg'v1: #Reg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  
  call out'0 := #CacheIR~useObjectId($objectId, ConsEmitPath(emitPath, 0));
  $objectReg'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $scratchReg'v1 := out'1;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestObjectIsProxy(false, $objectReg'v0, $scratchReg'v1, $failure'l0), ConsEmitPath(emitPath, 1));
  call #CacheIR~releaseReg($scratchReg'v1);
  return;
}

procedure #CacheIR~GuardInt32IsNonNegative($indexId: #Int32Id, emitPath: EmitPath)
{
  var $indexReg'v0: #Reg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($indexId, ConsEmitPath(emitPath, 0));
  $indexReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~Branch32Imm(#Condition^Variant~LessThan(), $indexReg'v0, 0bv32, $failure'l0), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~IsObjectResult($inputId: #ValueId, emitPath: EmitPath)
{
  var $scratchReg'v0: #Reg;
  var $valueReg'v1: #ValueReg;
  var out'0: #Reg;
  var out'1: #ValueReg;
  
  call out'0 := #CacheIR~allocateReg();
  $scratchReg'v0 := out'0;
  call out'1 := #CacheIR~useValueId($inputId, ConsEmitPath(emitPath, 0));
  $valueReg'v1 := out'1;
  call #MASM^emit(#MASM^Op~TestObjectSet(#Condition^Variant~Equal(), $valueReg'v1, $scratchReg'v0), ConsEmitPath(emitPath, 1));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Bool(), $scratchReg'v0, #CacheIR~outputReg), ConsEmitPath(emitPath, 2));
  call #CacheIR~releaseReg($scratchReg'v0);
  return;
}

procedure #CacheIR~GuardNonDoubleType($valueId: #ValueId, $type: #ValueType, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: EmitPath;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  if ($type == #ValueType^Variant~String()) {
    call #MASM^emit(#MASM^Op~BranchTestString(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  } else if ($type == #ValueType^Variant~Symbol()) {
    call #MASM^emit(#MASM^Op~BranchTestSymbol(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 2));
  } else if ($type == #ValueType^Variant~BigInt()) {
    call #MASM^emit(#MASM^Op~BranchTestBigInt(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 3));
  } else if ($type == #ValueType^Variant~Bool()) {
    call #MASM^emit(#MASM^Op~BranchTestBoolean(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 4));
  } else if ($type == #ValueType^Variant~Undefined()) {
    call #MASM^emit(#MASM^Op~BranchTestUndefined(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 5));
  } else if ($type == #ValueType^Variant~Null()) {
    call #MASM^emit(#MASM^Op~BranchTestNull(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 6));
  } else {
    assert false;
  }
  return;
}

procedure #CacheIR~GuardClass($objId: #ObjectId, $kind: #GuardClassKind, emitPath: EmitPath)
{
  var $objReg'v0: #Reg;
  var $scratchReg'v1: #Reg;
  var $class'v2: #Class;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Class;
  var out'3: #Class;
  var out'4: #Class;
  var out'5: #Class;
  var out'6: #Class;
  var out'7: #Class;
  var out'8: #Class;
  var out'9: #Class;
  var out'10: #Class;
  var out'11: #Class;
  
  call out'0 := #CacheIR~useObjectId($objId, ConsEmitPath(emitPath, 0));
  $objReg'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $scratchReg'v1 := out'1;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  if ($kind == #GuardClassKind^Variant~JSFunction()) {
    if (#CacheIR~objectGuardNeedsSpectreMitigations($objId)) {
      call #MASM^emit(#MASM^Op~BranchTestObjIsFunction(#Condition^Variant~NotEqual(), $objReg'v0, $scratchReg'v1, $objReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
    } else {
      call #MASM^emit(#MASM^Op~BranchTestObjIsFunctionNoSpectreMitigations(#Condition^Variant~NotEqual(), $objReg'v0, $scratchReg'v1, $failure'l0), ConsEmitPath(emitPath, 2));
    }
    return;
  }
  call out'2 := #ArrayObject~class();
  $class'v2 := out'2;
  if ($kind == #GuardClassKind^Variant~Array()) {
    call out'3 := #ArrayObject~class();
    $class'v2 := out'3;
  } else if ($kind == #GuardClassKind^Variant~PlainObject()) {
    call out'4 := #PlainObject~class();
    $class'v2 := out'4;
  } else if ($kind == #GuardClassKind^Variant~ArrayBuffer()) {
    call out'5 := #ArrayBufferObject~class();
    $class'v2 := out'5;
  } else if ($kind == #GuardClassKind^Variant~SharedArrayBuffer()) {
    call out'6 := #SharedArrayBufferObject~class();
    $class'v2 := out'6;
  } else if ($kind == #GuardClassKind^Variant~DataView()) {
    call out'7 := #DataViewObject~class();
    $class'v2 := out'7;
  } else if ($kind == #GuardClassKind^Variant~MappedArguments()) {
    call out'8 := #MappedArgumentsObject~class();
    $class'v2 := out'8;
  } else if ($kind == #GuardClassKind^Variant~UnmappedArguments()) {
    call out'9 := #UnmappedArgumentsObject~class();
    $class'v2 := out'9;
  } else if ($kind == #GuardClassKind^Variant~WindowProxy()) {
    $class'v2 := #Class~windowProxyClass();
  } else if ($kind == #GuardClassKind^Variant~Set()) {
    call out'10 := #SetObject~class();
    $class'v2 := out'10;
  } else if ($kind == #GuardClassKind^Variant~Map()) {
    call out'11 := #MapObject~class();
    $class'v2 := out'11;
  } else if ($kind == #GuardClassKind^Variant~JSFunction()) {
    assert false;
  }
  if (#CacheIR~objectGuardNeedsSpectreMitigations($objId)) {
    call #MASM^emit(#MASM^Op~BranchTestObjectClass(#Condition^Variant~NotEqual(), $objReg'v0, $class'v2, $scratchReg'v1, $objReg'v0, $failure'l0), ConsEmitPath(emitPath, 3));
  } else {
    call #MASM^emit(#MASM^Op~BranchTestObjectClassNoSpectreMitigations(#Condition^Variant~NotEqual(), $objReg'v0, $class'v2, $scratchReg'v1, $failure'l0), ConsEmitPath(emitPath, 4));
  }
  call #CacheIR~releaseReg($scratchReg'v1);
  return;
}

procedure #CacheIR~GuardBooleanToInt32($valueId: #ValueId, $int32Id: #Int32Id, emitPath: EmitPath)
{
  var $int32Reg'v0: #Reg;
  var $valueReg'v1: #ValueReg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #ValueReg;
  
  call out'0 := #CacheIR~defineInt32Id($int32Id);
  $int32Reg'v0 := out'0;
  call out'1 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v1 := out'1;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~FallibleUnboxBoolean($valueReg'v1, $int32Reg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  call #MASM^emit(#MASM^Op~CastBoolToInt32($int32Reg'v0), ConsEmitPath(emitPath, 2));
  return;
}

procedure #CacheIR~GuardToObject($valueId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: EmitPath;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestObject(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~GuardShape($objectId: #ObjectId, $shapeField: #ShapeField, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $shape'v1: #Shape;
  var $needsSpectreMitigations'v2: #Bool;
  var $scratchReg'v3: #Reg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  
  call out'0 := #CacheIR~useObjectId($objectId, ConsEmitPath(emitPath, 0));
  $objectReg'v0 := out'0;
  $shape'v1 := #CacheIR~readShapeField($shapeField);
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  $needsSpectreMitigations'v2 := #CacheIR~objectGuardNeedsSpectreMitigations($objectId);
  if ($needsSpectreMitigations'v2) {
    call out'1 := #CacheIR~allocateReg();
    $scratchReg'v3 := out'1;
    call #MASM^emit(#MASM^Op~BranchTestObjectShape(#Condition^Variant~NotEqual(), $objectReg'v0, $shape'v1, $scratchReg'v3, $objectReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
    call #CacheIR~releaseReg($scratchReg'v3);
  } else {
    call #MASM^emit(#MASM^Op~BranchTestObjectShapeNoSpectreMitigations(#Condition^Variant~NotEqual(), $objectReg'v0, $shape'v1, $failure'l0), ConsEmitPath(emitPath, 2));
  }
  return;
}

procedure #CacheIR~GuardAnyClass($objectId: #ObjectId, $classField: #ClassField, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $scratchReg'v1: #Reg;
  var $class'v2: #Class;
  var $needsSpectreMitigations'v3: #Bool;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  
  call out'0 := #CacheIR~useObjectId($objectId, ConsEmitPath(emitPath, 0));
  $objectReg'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $scratchReg'v1 := out'1;
  $class'v2 := #CacheIR~readClassField($classField);
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  $needsSpectreMitigations'v3 := #CacheIR~objectGuardNeedsSpectreMitigations($objectId);
  if ($needsSpectreMitigations'v3) {
    call #MASM^emit(#MASM^Op~BranchTestObjectClass(#Condition^Variant~NotEqual(), $objectReg'v0, $class'v2, $scratchReg'v1, $objectReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  } else {
    call #MASM^emit(#MASM^Op~BranchTestObjectClassNoSpectreMitigations(#Condition^Variant~NotEqual(), $objectReg'v0, $class'v2, $scratchReg'v1, $failure'l0), ConsEmitPath(emitPath, 2));
  }
  call #CacheIR~releaseReg($scratchReg'v1);
  return;
}

procedure #CacheIR~GuardFixedSlotValue($objId: #ObjectId, $offsetOffset: #Int32Field, $valOffset: #ValueField, emitPath: EmitPath)
{
  var $obj'v0: #Reg;
  var $offset'v1: #Reg;
  var $scratchVal'v2: #ValueReg;
  var $slotVal'v3: #BaseIndex;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #ValueReg;
  var out'3: #BaseIndex;
  
  call out'0 := #CacheIR~useObjectId($objId, ConsEmitPath(emitPath, 0));
  $obj'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $offset'v1 := out'1;
  call out'2 := #CacheIR~allocateValueReg();
  $scratchVal'v2 := out'2;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #CacheIR~emitLoadInt32StubField($offsetOffset, $offset'v1, ConsEmitPath(emitPath, 1));
  call #CacheIR~emitLoadValueStubField($valOffset, $scratchVal'v2, ConsEmitPath(emitPath, 2));
  call out'3 := #BaseIndex~new($obj'v0, $offset'v1, #Scale^Variant~TimesOne(), 0bv32);
  $slotVal'v3 := out'3;
  call #MASM^emit(#MASM^Op~BranchTestValue(#Condition^Variant~NotEqual(), $slotVal'v3, $scratchVal'v2, $failure'l0), ConsEmitPath(emitPath, 3));
  call #CacheIR~releaseReg($offset'v1);
  call #CacheIR~releaseValueReg($scratchVal'v2);
  return;
}

procedure #CacheIR~GuardDynamicSlotValue($objId: #ObjectId, $offsetOffset: #Int32Field, $valOffset: #ValueField, emitPath: EmitPath)
{
  var $obj'v0: #Reg;
  var $objSlots'v1: #Reg;
  var $offset'v2: #Reg;
  var $scratchVal'v3: #ValueReg;
  var $objOffsetAddr'v4: #Address;
  var $slotVal'v5: #BaseIndex;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  var out'3: #ValueReg;
  var out'4: #Address;
  var out'5: #BaseIndex;
  
  call out'0 := #CacheIR~useObjectId($objId, ConsEmitPath(emitPath, 0));
  $obj'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $objSlots'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $offset'v2 := out'2;
  call out'3 := #CacheIR~allocateValueReg();
  $scratchVal'v3 := out'3;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call out'4 := #Address~new($obj'v0, #UInt64^to#Int32(#NativeObject~offsetOfSlots()));
  $objOffsetAddr'v4 := out'4;
  call #MASM^emit(#MASM^Op~LoadPtrAddress($objOffsetAddr'v4, $objSlots'v1), ConsEmitPath(emitPath, 1));
  call #CacheIR~emitLoadInt32StubField($offsetOffset, $offset'v2, ConsEmitPath(emitPath, 2));
  call #CacheIR~emitLoadValueStubField($valOffset, $scratchVal'v3, ConsEmitPath(emitPath, 3));
  call out'5 := #BaseIndex~new($objSlots'v1, $offset'v2, #Scale^Variant~TimesOne(), 0bv32);
  $slotVal'v5 := out'5;
  call #MASM^emit(#MASM^Op~BranchTestValue(#Condition^Variant~NotEqual(), $slotVal'v5, $scratchVal'v3, $failure'l0), ConsEmitPath(emitPath, 4));
  call #CacheIR~releaseReg($objSlots'v1);
  call #CacheIR~releaseReg($offset'v2);
  call #CacheIR~releaseValueReg($scratchVal'v3);
  return;
}

procedure #CacheIR~GuardToString($valueId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: EmitPath;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestString(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~GuardToSymbol($valueId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: EmitPath;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestSymbol(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~GuardToInt32($valueId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: EmitPath;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestInt32(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~GuardToBoolean($valueId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: EmitPath;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestBoolean(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~GuardToBigInt($valueId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: EmitPath;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useValueId($valueId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestBigInt(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~GuardNoDenseElements($objectId: #ObjectId, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $scratchReg'v1: #Reg;
  var $initLength'v2: #Address;
  var $tmp'v3: #Address;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Address;
  var out'3: #Address;
  
  call out'0 := #CacheIR~useObjectId($objectId, ConsEmitPath(emitPath, 0));
  $objectReg'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $scratchReg'v1 := out'1;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call out'2 := #Address~new($objectReg'v0, #UInt64^to#Int32(#NativeObject~offsetOfElements()));
  $tmp'v3 := out'2;
  call #MASM^emit(#MASM^Op~LoadPtrAddress($tmp'v3, $scratchReg'v1), ConsEmitPath(emitPath, 1));
  call out'3 := #Address~new($scratchReg'v1, #NativeObjectElements~offsetOfInitializedLength());
  $initLength'v2 := out'3;
  call #MASM^emit(#MASM^Op~Branch32AddressImm32(#Condition^Variant~NotEqual(), $initLength'v2, 0bv32, $failure'l0), ConsEmitPath(emitPath, 2));
  call #CacheIR~releaseReg($scratchReg'v1);
  return;
}

procedure #CacheIR~BooleanToNumber($boolId: #BoolId, $numberId: #NumberId, emitPath: EmitPath)
{
  var $boolReg'v0: #Reg;
  var $numberReg'v1: #ValueReg;
  var out'0: #Reg;
  var out'1: #ValueReg;
  
  call out'0 := #CacheIR~useBoolId($boolId, ConsEmitPath(emitPath, 0));
  $boolReg'v0 := out'0;
  call out'1 := #CacheIR~useNumberId($numberId, ConsEmitPath(emitPath, 1));
  $numberReg'v1 := out'1;
  call #MASM^emit(#MASM^Op~CastBoolToInt32($boolReg'v0), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $boolReg'v0, $numberReg'v1), ConsEmitPath(emitPath, 3));
  return;
}

procedure #CacheIR~LoadObject($resultId: #ObjectId, $objField: #ObjectField, emitPath: EmitPath)
{
  var $resultReg'v0: #Reg;
  var out'0: #Reg;
  
  call out'0 := #CacheIR~defineObjectId($resultId);
  $resultReg'v0 := out'0;
  call #CacheIR~emitLoadObjectStubField($objField, $resultReg'v0, ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~LoadProto($objectId: #ObjectId, $resultId: #ObjectId, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $resultReg'v1: #Reg;
  var out'0: #Reg;
  var out'1: #Reg;
  
  call out'0 := #CacheIR~useObjectId($objectId, ConsEmitPath(emitPath, 0));
  $objectReg'v0 := out'0;
  call out'1 := #CacheIR~defineObjectId($resultId);
  $resultReg'v1 := out'1;
  call #MASM^emit(#MASM^Op~LoadObjectProto($objectReg'v0, $resultReg'v1), ConsEmitPath(emitPath, 1));
  call #MASM^emit(#MASM^Op~UnboxObjectProto($resultReg'v1, $resultReg'v1), ConsEmitPath(emitPath, 2));
  return;
}

procedure #CacheIR~LoadFixedSlotResult($objectId: #ObjectId, $slotField: #Int32Field, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $slotOffset'v1: #Int32;
  var $tmp'v2: #Address;
  var out'0: #Reg;
  var out'1: #Address;
  
  call out'0 := #CacheIR~useObjectId($objectId, ConsEmitPath(emitPath, 0));
  $objectReg'v0 := out'0;
  $slotOffset'v1 := #CacheIR~readInt32Field($slotField);
  call out'1 := #Address~new($objectReg'v0, $slotOffset'v1);
  $tmp'v2 := out'1;
  call #MASM^emit(#MASM^Op~LoadValueAddress($tmp'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~LoadDynamicSlotResult($objectId: #ObjectId, $slotField: #Int32Field, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $slotOffset'v1: #Int32;
  var $scratchReg'v2: #Reg;
  var $tmp'v3: #Address;
  var $tmp'v4: #Address;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Address;
  var out'3: #Address;
  
  call out'0 := #CacheIR~useObjectId($objectId, ConsEmitPath(emitPath, 0));
  $objectReg'v0 := out'0;
  $slotOffset'v1 := #CacheIR~readInt32Field($slotField);
  call out'1 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'1;
  call out'2 := #Address~new($objectReg'v0, #UInt64^to#Int32(#NativeObject~offsetOfSlots()));
  $tmp'v3 := out'2;
  call #MASM^emit(#MASM^Op~LoadPtrAddress($tmp'v3, $scratchReg'v2), ConsEmitPath(emitPath, 1));
  call out'3 := #Address~new($scratchReg'v2, $slotOffset'v1);
  $tmp'v4 := out'3;
  call #MASM^emit(#MASM^Op~LoadValueAddress($tmp'v4, #CacheIR~outputReg), ConsEmitPath(emitPath, 2));
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~LoadInt32ArrayLengthResult($objectId: #ObjectId, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $scratchReg'v1: #Reg;
  var $tmp'v2: #Address;
  var $tmp'v3: #Address;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Address;
  var out'3: #Address;
  
  call out'0 := #CacheIR~useObjectId($objectId, ConsEmitPath(emitPath, 0));
  $objectReg'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $scratchReg'v1 := out'1;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call out'2 := #Address~new($objectReg'v0, #UInt64^to#Int32(#NativeObject~offsetOfElements()));
  $tmp'v2 := out'2;
  call #MASM^emit(#MASM^Op~LoadPtrAddress($tmp'v2, $scratchReg'v1), ConsEmitPath(emitPath, 1));
  call out'3 := #Address~new($scratchReg'v1, #NativeObjectElements~offsetOfLength());
  $tmp'v3 := out'3;
  call #MASM^emit(#MASM^Op~Load32Address($tmp'v3, $scratchReg'v1), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~BranchTest32(#Condition^Variant~Signed(), $scratchReg'v1, $scratchReg'v1, $failure'l0), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v1, #CacheIR~outputReg), ConsEmitPath(emitPath, 4));
  call #CacheIR~releaseReg($scratchReg'v1);
  return;
}

procedure #CacheIR~LoadArgumentsObjectLengthResult($objectId: #ObjectId, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $argObjLengthReg'v1: #Reg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  
  call out'0 := #CacheIR~useObjectId($objectId, ConsEmitPath(emitPath, 0));
  $objectReg'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $argObjLengthReg'v1 := out'1;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~LoadArgumentsObjectLength($objectReg'v0, $argObjLengthReg'v1, $failure'l0), ConsEmitPath(emitPath, 1));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $argObjLengthReg'v1, #CacheIR~outputReg), ConsEmitPath(emitPath, 2));
  call #CacheIR~releaseReg($argObjLengthReg'v1);
  return;
}

procedure #CacheIR~LoadInstanceOfObjectResult($lhsId: #ValueId, $protoId: #ObjectId, emitPath: EmitPath)
{
  var $lhsReg'v0: #ValueReg;
  var $protoReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var $failure'l0: EmitPath;
  var $returnFalse'l1: EmitPath;
  var $returnTrue'l2: EmitPath;
  var $done'l3: EmitPath;
  var $loopHead'l4: EmitPath;
  var out'0: #ValueReg;
  var out'1: #Reg;
  var out'2: #Reg;
  
  call out'0 := #CacheIR~useValueId($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useObjectId($protoId, ConsEmitPath(emitPath, 1));
  $protoReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'2;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  $returnFalse'l1 := ConsEmitPath(emitPath, 1);
  $returnTrue'l2 := ConsEmitPath(emitPath, 2);
  $done'l3 := ConsEmitPath(emitPath, 3);
  call #MASM^emit(#MASM^Op~FallibleUnboxObject($lhsReg'v0, $scratchReg'v2, $failure'l0), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~LoadObjectProto($scratchReg'v2, $scratchReg'v2), ConsEmitPath(emitPath, 3));
  $loopHead'l4 := ConsEmitPath(emitPath, 4);
  call #MASM^bind($loopHead'l4);
  call #MASM^emit(#MASM^Op~BranchTestNullProto($scratchReg'v2, $returnFalse'l1), ConsEmitPath(emitPath, 4));
  call #MASM^emit(#MASM^Op~BranchTestLazyProto($scratchReg'v2, $failure'l0), ConsEmitPath(emitPath, 5));
  call #MASM^emit(#MASM^Op~UnboxObjectProto($scratchReg'v2, $scratchReg'v2), ConsEmitPath(emitPath, 6));
  call #MASM^emit(#MASM^Op~BranchObject(#Condition^Variant~Equal(), $scratchReg'v2, $protoReg'v1, $returnTrue'l2), ConsEmitPath(emitPath, 7));
  call #MASM^emit(#MASM^Op~LoadObjectProto($scratchReg'v2, $scratchReg'v2), ConsEmitPath(emitPath, 8));
  call #MASM^emit(#MASM^Op~Jump($loopHead'l4), ConsEmitPath(emitPath, 9));
  call #MASM^bind($returnFalse'l1);
  call #MASM^emit(#MASM^Op~StoreBoolValue(false, #CacheIR~outputReg), ConsEmitPath(emitPath, 10));
  call #MASM^emit(#MASM^Op~Jump($done'l3), ConsEmitPath(emitPath, 11));
  call #MASM^bind($returnTrue'l2);
  call #MASM^emit(#MASM^Op~StoreBoolValue(true, #CacheIR~outputReg), ConsEmitPath(emitPath, 12));
  call #MASM^bind($done'l3);
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~LoadStringResult($stringId: #StringId, emitPath: EmitPath)
{
  var $stringReg'v0: #Reg;
  var out'0: #Reg;
  
  call out'0 := #CacheIR~useStringId($stringId, ConsEmitPath(emitPath, 0));
  $stringReg'v0 := out'0;
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~String(), $stringReg'v0, #CacheIR~outputReg), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~LoadSymbolResult($symbolId: #SymbolId, emitPath: EmitPath)
{
  var $symbolReg'v0: #Reg;
  var out'0: #Reg;
  
  call out'0 := #CacheIR~useSymbolId($symbolId, ConsEmitPath(emitPath, 0));
  $symbolReg'v0 := out'0;
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Symbol(), $symbolReg'v0, #CacheIR~outputReg), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~LoadInt32Result($int32Id: #Int32Id, emitPath: EmitPath)
{
  var $int32Reg'v0: #Reg;
  var out'0: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($int32Id, ConsEmitPath(emitPath, 0));
  $int32Reg'v0 := out'0;
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $int32Reg'v0, #CacheIR~outputReg), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~LoadBooleanResult($bool: #Bool, emitPath: EmitPath)
{
  var $value'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #Value~fromBool($bool);
  $value'v0 := out'0;
  call #MASM^emit(#MASM^Op~MoveValueImm($value'v0, #CacheIR~outputReg), ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~LoadUndefinedResult(emitPath: EmitPath)
{
  var $value'v0: #Value;
  var out'0: #Value;
  
  call out'0 := #Value~getUndefined();
  $value'v0 := out'0;
  call #MASM^emit(#MASM^Op~MoveValueImm($value'v0, #CacheIR~outputReg), ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~LoadBigIntResult($bigIntId: #BigIntId, emitPath: EmitPath)
{
  var $bigIntReg'v0: #Reg;
  var out'0: #Reg;
  
  call out'0 := #CacheIR~useBigIntId($bigIntId, ConsEmitPath(emitPath, 0));
  $bigIntReg'v0 := out'0;
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~BigInt(), $bigIntReg'v0, #CacheIR~outputReg), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~LoadDoubleResult($numberId: #NumberId, emitPath: EmitPath)
{
  var $numberReg'v0: #ValueReg;
  var out'0: #ValueReg;
  
  call out'0 := #CacheIR~useNumberId($numberId, ConsEmitPath(emitPath, 0));
  $numberReg'v0 := out'0;
  call #MASM^emit(#MASM^Op~MoveValue($numberReg'v0, #CacheIR~outputReg), ConsEmitPath(emitPath, 1));
  call #MASM^emit(#MASM^Op~ConvertInt32ValueToDouble(#CacheIR~outputReg), ConsEmitPath(emitPath, 2));
  return;
}

procedure #CacheIR~LoadInt32Constant($valField: #Int32Field, $resultId: #Int32Id, emitPath: EmitPath)
{
  var $resultReg'v0: #Reg;
  var out'0: #Reg;
  
  call out'0 := #CacheIR~defineInt32Id($resultId);
  $resultReg'v0 := out'0;
  call #CacheIR~emitLoadInt32StubField($valField, $resultReg'v0, ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~LoadInt32TruthyResult($inputId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $tmp'v1: #Value;
  var $tmp'v2: #Value;
  var $ifFalse'l0: EmitPath;
  var $done'l1: EmitPath;
  var out'0: #ValueReg;
  var out'1: #Value;
  var out'2: #Value;
  
  call out'0 := #CacheIR~useValueId($inputId, ConsEmitPath(emitPath, 0));
  $valueReg'v0 := out'0;
  $ifFalse'l0 := ConsEmitPath(emitPath, 0);
  $done'l1 := ConsEmitPath(emitPath, 1);
  call #MASM^emit(#MASM^Op~BranchTestInt32Truthy(false, $valueReg'v0, $ifFalse'l0), ConsEmitPath(emitPath, 1));
  call out'1 := #Value~fromBool(true);
  $tmp'v1 := out'1;
  call #MASM^emit(#MASM^Op~MoveValueImm($tmp'v1, #CacheIR~outputReg), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~Jump($done'l1), ConsEmitPath(emitPath, 3));
  call #MASM^bind($ifFalse'l0);
  call out'2 := #Value~fromBool(false);
  $tmp'v2 := out'2;
  call #MASM^emit(#MASM^Op~MoveValueImm($tmp'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 4));
  call #MASM^bind($done'l1);
  return;
}

procedure #CacheIR~Int32AddResult($lhsId: #Int32Id, $rhsId: #Int32Id, emitPath: EmitPath)
{
  var $lhsReg'v0: #Reg;
  var $rhsReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($rhsId, ConsEmitPath(emitPath, 1));
  $rhsReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'2;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~Mov($rhsReg'v1, $scratchReg'v2), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~BranchAdd32(#Condition^Variant~Overflow(), $lhsReg'v0, $scratchReg'v2, $failure'l0), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 4));
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~Int32SubResult($lhsId: #Int32Id, $rhsId: #Int32Id, emitPath: EmitPath)
{
  var $lhsReg'v0: #Reg;
  var $rhsReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($rhsId, ConsEmitPath(emitPath, 1));
  $rhsReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'2;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~Mov($lhsReg'v0, $scratchReg'v2), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~BranchSub32(#Condition^Variant~Overflow(), $rhsReg'v1, $scratchReg'v2, $failure'l0), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 4));
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~Int32MulResult($lhsId: #Int32Id, $rhsId: #Int32Id, emitPath: EmitPath)
{
  var $lhsReg'v0: #Reg;
  var $rhsReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var $scratch2Reg'v3: #Reg;
  var $failure'l0: EmitPath;
  var $maybeNegZero'l1: EmitPath;
  var $done'l2: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  var out'3: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($rhsId, ConsEmitPath(emitPath, 1));
  $rhsReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'2;
  call out'3 := #CacheIR~allocateReg();
  $scratch2Reg'v3 := out'3;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  $maybeNegZero'l1 := ConsEmitPath(emitPath, 1);
  $done'l2 := ConsEmitPath(emitPath, 2);
  call #MASM^emit(#MASM^Op~Mov($lhsReg'v0, $scratchReg'v2), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~BranchMul32(#Condition^Variant~Overflow(), $rhsReg'v1, $scratchReg'v2, $failure'l0), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~BranchTest32(#Condition^Variant~Zero(), $scratchReg'v2, $scratchReg'v2, $maybeNegZero'l1), ConsEmitPath(emitPath, 4));
  call #MASM^emit(#MASM^Op~Jump($done'l2), ConsEmitPath(emitPath, 5));
  call #MASM^bind($maybeNegZero'l1);
  call #MASM^emit(#MASM^Op~Mov($lhsReg'v0, $scratch2Reg'v3), ConsEmitPath(emitPath, 6));
  call #MASM^emit(#MASM^Op~Or32($rhsReg'v1, $scratch2Reg'v3), ConsEmitPath(emitPath, 7));
  call #MASM^emit(#MASM^Op~BranchTest32(#Condition^Variant~Signed(), $scratch2Reg'v3, $scratch2Reg'v3, $failure'l0), ConsEmitPath(emitPath, 8));
  call #MASM^bind($done'l2);
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 9));
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~Int32DivResult($lhsId: #Int32Id, $rhsId: #Int32Id, emitPath: EmitPath)
{
  var $lhsReg'v0: #Reg;
  var $rhsReg'v1: #Reg;
  var $remReg'v2: #Reg;
  var $scratchReg'v3: #Reg;
  var $failure'l0: EmitPath;
  var $notOverflow'l1: EmitPath;
  var $notZero'l2: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  var out'3: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($rhsId, ConsEmitPath(emitPath, 1));
  $rhsReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $remReg'v2 := out'2;
  call out'3 := #CacheIR~allocateReg();
  $scratchReg'v3 := out'3;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTest32(#Condition^Variant~Zero(), $rhsReg'v1, $rhsReg'v1, $failure'l0), ConsEmitPath(emitPath, 2));
  $notOverflow'l1 := ConsEmitPath(emitPath, 1);
  call #MASM^emit(#MASM^Op~Branch32Imm(#Condition^Variant~NotEqual(), $lhsReg'v0, #Int32^sub(2147483649bv32, 1bv32), $notOverflow'l1), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~Branch32Imm(#Condition^Variant~Equal(), $rhsReg'v1, 4294967295bv32, $failure'l0), ConsEmitPath(emitPath, 4));
  call #MASM^bind($notOverflow'l1);
  $notZero'l2 := ConsEmitPath(emitPath, 2);
  call #MASM^emit(#MASM^Op~BranchTest32(#Condition^Variant~NonZero(), $lhsReg'v0, $lhsReg'v0, $notZero'l2), ConsEmitPath(emitPath, 5));
  call #MASM^emit(#MASM^Op~BranchTest32(#Condition^Variant~Signed(), $rhsReg'v1, $rhsReg'v1, $failure'l0), ConsEmitPath(emitPath, 6));
  call #MASM^bind($notZero'l2);
  call #MASM^emit(#MASM^Op~Mov($lhsReg'v0, $scratchReg'v3), ConsEmitPath(emitPath, 7));
  call #MASM^emit(#MASM^Op~FlexibleDivMod32($rhsReg'v1, $scratchReg'v3, $remReg'v2), ConsEmitPath(emitPath, 8));
  call #MASM^emit(#MASM^Op~BranchTest32(#Condition^Variant~NonZero(), $remReg'v2, $remReg'v2, $failure'l0), ConsEmitPath(emitPath, 9));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v3, #CacheIR~outputReg), ConsEmitPath(emitPath, 10));
  call #CacheIR~releaseReg($remReg'v2);
  call #CacheIR~releaseReg($scratchReg'v3);
  return;
}

procedure #CacheIR~Int32ModResult($lhsId: #Int32Id, $rhsId: #Int32Id, emitPath: EmitPath)
{
  var $lhsReg'v0: #Reg;
  var $rhsReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var $failure'l0: EmitPath;
  var $notOverflow'l1: EmitPath;
  var $notZero'l2: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($rhsId, ConsEmitPath(emitPath, 1));
  $rhsReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'2;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTest32(#Condition^Variant~Zero(), $rhsReg'v1, $rhsReg'v1, $failure'l0), ConsEmitPath(emitPath, 2));
  $notOverflow'l1 := ConsEmitPath(emitPath, 1);
  call #MASM^emit(#MASM^Op~Branch32Imm(#Condition^Variant~NotEqual(), $lhsReg'v0, #Int32^sub(2147483649bv32, 1bv32), $notOverflow'l1), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~Branch32Imm(#Condition^Variant~Equal(), $rhsReg'v1, 4294967295bv32, $failure'l0), ConsEmitPath(emitPath, 4));
  call #MASM^bind($notOverflow'l1);
  call #MASM^emit(#MASM^Op~Mov($lhsReg'v0, $scratchReg'v2), ConsEmitPath(emitPath, 5));
  call #MASM^emit(#MASM^Op~FlexibleRemainder32($rhsReg'v1, $scratchReg'v2), ConsEmitPath(emitPath, 6));
  $notZero'l2 := ConsEmitPath(emitPath, 2);
  call #MASM^emit(#MASM^Op~BranchTest32(#Condition^Variant~NonZero(), $scratchReg'v2, $scratchReg'v2, $notZero'l2), ConsEmitPath(emitPath, 7));
  call #MASM^emit(#MASM^Op~BranchTest32(#Condition^Variant~Signed(), $lhsReg'v0, $lhsReg'v0, $failure'l0), ConsEmitPath(emitPath, 8));
  call #MASM^bind($notZero'l2);
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 9));
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~Int32BitOrResult($lhsId: #Int32Id, $rhsId: #Int32Id, emitPath: EmitPath)
{
  var $lhsReg'v0: #Reg;
  var $rhsReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($rhsId, ConsEmitPath(emitPath, 1));
  $rhsReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'2;
  call #MASM^emit(#MASM^Op~Mov($rhsReg'v1, $scratchReg'v2), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~Or32($lhsReg'v0, $scratchReg'v2), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 4));
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~Int32BitXorResult($lhsId: #Int32Id, $rhsId: #Int32Id, emitPath: EmitPath)
{
  var $lhsReg'v0: #Reg;
  var $rhsReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($rhsId, ConsEmitPath(emitPath, 1));
  $rhsReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'2;
  call #MASM^emit(#MASM^Op~Mov($rhsReg'v1, $scratchReg'v2), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~Xor32($lhsReg'v0, $scratchReg'v2), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 4));
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~Int32BitAndResult($lhsId: #Int32Id, $rhsId: #Int32Id, emitPath: EmitPath)
{
  var $lhsReg'v0: #Reg;
  var $rhsReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($rhsId, ConsEmitPath(emitPath, 1));
  $rhsReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'2;
  call #MASM^emit(#MASM^Op~Mov($rhsReg'v1, $scratchReg'v2), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~And32($lhsReg'v0, $scratchReg'v2), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 4));
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~Int32LeftShiftResult($lhsId: #Int32Id, $rhsId: #Int32Id, emitPath: EmitPath)
{
  var $lhsReg'v0: #Reg;
  var $rhsReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($rhsId, ConsEmitPath(emitPath, 1));
  $rhsReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'2;
  call #MASM^emit(#MASM^Op~Mov($lhsReg'v0, $scratchReg'v2), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~FlexibleLshift32($rhsReg'v1, $scratchReg'v2), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 4));
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~Int32RightShiftResult($lhsId: #Int32Id, $rhsId: #Int32Id, emitPath: EmitPath)
{
  var $lhsReg'v0: #Reg;
  var $rhsReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($rhsId, ConsEmitPath(emitPath, 1));
  $rhsReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'2;
  call #MASM^emit(#MASM^Op~Mov($lhsReg'v0, $scratchReg'v2), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~FlexibleRshift32Arithmetic($rhsReg'v1, $scratchReg'v2), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 4));
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~Int32URightShiftResult($lhsId: #Int32Id, $rhsId: #Int32Id, emitPath: EmitPath)
{
  var $lhsReg'v0: #Reg;
  var $rhsReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($lhsId, ConsEmitPath(emitPath, 0));
  $lhsReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($rhsId, ConsEmitPath(emitPath, 1));
  $rhsReg'v1 := out'1;
  call out'2 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'2;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~Mov($lhsReg'v0, $scratchReg'v2), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~FlexibleRshift32($rhsReg'v1, $scratchReg'v2), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~BranchTest32(#Condition^Variant~Signed(), $scratchReg'v2, $scratchReg'v2, $failure'l0), ConsEmitPath(emitPath, 4));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v2, #CacheIR~outputReg), ConsEmitPath(emitPath, 5));
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheIR~Int32NegationResult($inputId: #Int32Id, emitPath: EmitPath)
{
  var $inputReg'v0: #Reg;
  var $scratchReg'v1: #Reg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($inputId, ConsEmitPath(emitPath, 0));
  $inputReg'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $scratchReg'v1 := out'1;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTest32Imm(#Condition^Variant~Zero(), $inputReg'v0, 0bv32, $failure'l0), ConsEmitPath(emitPath, 1));
  call #MASM^emit(#MASM^Op~Mov($inputReg'v0, $scratchReg'v1), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~Neg32($scratchReg'v1), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v1, #CacheIR~outputReg), ConsEmitPath(emitPath, 4));
  call #CacheIR~releaseReg($scratchReg'v1);
  return;
}

procedure #CacheIR~Int32IncResult($inputId: #Int32Id, emitPath: EmitPath)
{
  var $inputReg'v0: #Reg;
  var $scratchReg'v1: #Reg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($inputId, ConsEmitPath(emitPath, 0));
  $inputReg'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $scratchReg'v1 := out'1;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~Mov($inputReg'v0, $scratchReg'v1), ConsEmitPath(emitPath, 1));
  call #MASM^emit(#MASM^Op~BranchAdd32Imm(#Condition^Variant~Overflow(), 1bv32, $scratchReg'v1, $failure'l0), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v1, #CacheIR~outputReg), ConsEmitPath(emitPath, 3));
  call #CacheIR~releaseReg($scratchReg'v1);
  return;
}

procedure #CacheIR~Int32DecResult($inputId: #Int32Id, emitPath: EmitPath)
{
  var $inputReg'v0: #Reg;
  var $scratchReg'v1: #Reg;
  var $failure'l0: EmitPath;
  var out'0: #Reg;
  var out'1: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($inputId, ConsEmitPath(emitPath, 0));
  $inputReg'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $scratchReg'v1 := out'1;
  $failure'l0 := ConsEmitPath(emitPath, 0);
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~Mov($inputReg'v0, $scratchReg'v1), ConsEmitPath(emitPath, 1));
  call #MASM^emit(#MASM^Op~BranchAdd32Imm(#Condition^Variant~Overflow(), 1bv32, $scratchReg'v1, $failure'l0), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v1, #CacheIR~outputReg), ConsEmitPath(emitPath, 3));
  call #CacheIR~releaseReg($scratchReg'v1);
  return;
}

procedure #CacheIR~Int32NotResult($inputId: #Int32Id, emitPath: EmitPath)
{
  var $inputReg'v0: #Reg;
  var $scratchReg'v1: #Reg;
  var out'0: #Reg;
  var out'1: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($inputId, ConsEmitPath(emitPath, 0));
  $inputReg'v0 := out'0;
  call out'1 := #CacheIR~allocateReg();
  $scratchReg'v1 := out'1;
  call #MASM^emit(#MASM^Op~Mov($inputReg'v0, $scratchReg'v1), ConsEmitPath(emitPath, 1));
  call #MASM^emit(#MASM^Op~Not32($scratchReg'v1), ConsEmitPath(emitPath, 2));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $scratchReg'v1, #CacheIR~outputReg), ConsEmitPath(emitPath, 3));
  call #CacheIR~releaseReg($scratchReg'v1);
  return;
}

procedure #CacheIR~Int32MinMax($isMax: #Bool, $firstId: #Int32Id, $secondId: #Int32Id, $resultId: #Int32Id, emitPath: EmitPath)
{
  var $firstReg'v0: #Reg;
  var $secondReg'v1: #Reg;
  var $resultReg'v2: #Reg;
  var out'0: #Reg;
  var out'1: #Reg;
  var out'2: #Reg;
  
  call out'0 := #CacheIR~useInt32Id($firstId, ConsEmitPath(emitPath, 0));
  $firstReg'v0 := out'0;
  call out'1 := #CacheIR~useInt32Id($secondId, ConsEmitPath(emitPath, 1));
  $secondReg'v1 := out'1;
  call out'2 := #CacheIR~defineInt32Id($resultId);
  $resultReg'v2 := out'2;
  call #MASM^emit(#MASM^Op~Mov($firstReg'v0, $resultReg'v2), ConsEmitPath(emitPath, 2));
  if ($isMax) {
    call #MASM^emit(#MASM^Op~Cmp32Move32(#Condition^Variant~GreaterThan(), $secondReg'v1, $firstReg'v0, $secondReg'v1, $resultReg'v2), ConsEmitPath(emitPath, 3));
  } else {
    call #MASM^emit(#MASM^Op~Cmp32Move32(#Condition^Variant~LessThan(), $secondReg'v1, $firstReg'v0, $secondReg'v1, $resultReg'v2), ConsEmitPath(emitPath, 4));
  }
  return;
}

procedure #CacheStub~GetProp(emitPath: EmitPath)
{
  var $tmp'v0: #OperandId;
  var $tmp'v1: #ValueId;
  var $tmp'v2: #ShapeField;
  var $tmp'v3: #Shape;
  var $tmp'v4: #Shape;
  var $tmp'v5: #Int32Field;
  var $tmp'v6: #Int32;
  var $tmp'v7: #Shape;
  var $tmp'v8: #BaseShape;
  var $tmp'v9: #BaseShape;
  var $tmp'v10: #Shape;
  var $tmp'v11: #Class;
  var $tmp'v12: #Class;
  var $tmp'v13: #Shape;
  var $tmp'v14: #UInt32;
  var $tmp'v15: #Shape;
  var $tmp'v16: #UInt32;
  var $tmp'v17: #BaseShape;
  var $tmp'v18: #TaggedProto;
  var $tmp'v19: #TaggedProto;
  var $tmp'v20: #Class;
  var $tmp'v21: #TaggedProto;
  var $tmp'v22: #ValueId;
  var $tmp'v23: #ObjectId;
  var $tmp'v24: #ShapeField;
  var $tmp'v25: #ObjectId;
  var $tmp'v26: #Int32Field;
  var out'0: #OperandId;
  var out'1: #ValueId;
  var out'2: #Class;
  var out'3: #TaggedProto;
  var out'4: #Bool;
  var out'5: #ValueId;
  var out'6: #ObjectId;
  var out'7: #ObjectId;
  
  call out'0 := #OperandId~fromId(0bv16);
  $tmp'v0 := out'0;
  call #initOperandId($tmp'v0);
  call out'1 := #ValueId~fromId(0bv16);
  $tmp'v1 := out'1;
  call #initInputValueId($tmp'v1);
  $tmp'v2 := #ShapeField~fromOffset(0bv32);
  $tmp'v3 := #CacheIR~readShapeField($tmp'v2);
  $tmp'v4 := #Shape~fromAddr(57504657821056bv64);
  assume ($tmp'v3 == $tmp'v4);
  $tmp'v5 := #Int32Field~fromOffset(8bv32);
  $tmp'v6 := #CacheIR~readInt32Field($tmp'v5);
  assume ($tmp'v6 == 24bv32);
  $tmp'v7 := #Shape~fromAddr(57504657821056bv64);
  $tmp'v8 := #Shape~baseShapeOf($tmp'v7);
  $tmp'v9 := #BaseShape~fromAddr(17150383721104bv64);
  assume ($tmp'v8 == $tmp'v9);
  $tmp'v10 := #Shape~fromAddr(57504657821056bv64);
  call out'2 := #Shape~classOf($tmp'v10);
  $tmp'v11 := out'2;
  $tmp'v12 := #Class~fromAddr(139810476811216bv64);
  assume ($tmp'v11 == $tmp'v12);
  $tmp'v13 := #Shape~fromAddr(57504657821056bv64);
  $tmp'v14 := #Shape~numFixedSlots($tmp'v13);
  assume ($tmp'v14 == 4bv32);
  $tmp'v15 := #Shape~fromAddr(57504657821056bv64);
  $tmp'v16 := #Shape~slotSpan($tmp'v15);
  assume ($tmp'v16 == 16bv32);
  $tmp'v17 := #BaseShape~fromAddr(17150383721104bv64);
  call out'3 := #BaseShape~protoOf($tmp'v17);
  $tmp'v18 := out'3;
  $tmp'v19 := #TaggedProto~fromAddr(17150383679136bv64);
  assume ($tmp'v18 == $tmp'v19);
  $tmp'v20 := #Class~fromAddr(139810476811216bv64);
  assume #Class~isNativeObject($tmp'v20);
  $tmp'v21 := #TaggedProto~fromAddr(17150383679136bv64);
  call out'4 := #TaggedProto~isObject($tmp'v21);
  assume out'4;
  call out'5 := #ValueId~fromId(0bv16);
  $tmp'v22 := out'5;
  call #CacheIR~GuardToObject($tmp'v22, ConsEmitPath(emitPath, 0));
  call out'6 := #ObjectId~fromId(0bv16);
  $tmp'v23 := out'6;
  $tmp'v24 := #ShapeField~fromOffset(0bv32);
  call #CacheIR~GuardShape($tmp'v23, $tmp'v24, ConsEmitPath(emitPath, 1));
  call out'7 := #ObjectId~fromId(0bv16);
  $tmp'v25 := out'7;
  $tmp'v26 := #Int32Field~fromOffset(8bv32);
  call #CacheIR~LoadFixedSlotResult($tmp'v25, $tmp'v26, ConsEmitPath(emitPath, 2));
  call #CacheIR~ReturnFromIC(ConsEmitPath(emitPath, 3));
  return;
}

procedure {:entrypoint} EntryPoint#CacheStub~GetProp()
{
  var op: #MASM^Op;
  
  #MASM^emitPath := NilEmitPath();
  call #CacheStub~GetProp(NilEmitPath());
  call #MASM^emit(#MASM^Op^Exit(), NilEmitPath());
  #MASM^emitPath := #MASM^nextEmitPath(NilEmitPath());
  goto emit'0#CacheIR~GuardToObject'0#CacheIR~useValueId'0#MASM~TagValue, emit'0#CacheIR~GuardToObject'1#MASM~BranchTestObject;
  
  emit'0#CacheIR~GuardToObject'0#CacheIR~useValueId'0#MASM~TagValue:
  assume {:partition} (#MASM^emitPath == ConsEmitPath(ConsEmitPath(ConsEmitPath(NilEmitPath(), 0), 0), 0));
  op := #MASM^ops(#MASM^emitPath);
  call #MASM~TagValue($valTy##MASM^Op~TagValue(op), $payload##MASM^Op~TagValue(op), $dest##MASM^Op~TagValue(op));
  goto emit'0#CacheIR~GuardToObject'1#MASM~BranchTestObject;
  
  emit'0#CacheIR~GuardToObject'1#MASM~BranchTestObject:
  assume {:partition} (#MASM^emitPath == ConsEmitPath(ConsEmitPath(NilEmitPath(), 0), 1));
  op := #MASM^ops(#MASM^emitPath);
  call #MASM~BranchTestObject($condition##MASM^Op~BranchTestObject(op), $valueReg##MASM^Op~BranchTestObject(op), $branch##MASM^Op~BranchTestObject(op));
  goto emit^Exit, emit'1#CacheIR~GuardShape'0#CacheIR~useObjectId'0#CacheIR~useTypedId'0#MASM~UnboxNonDouble, emit'1#CacheIR~GuardShape'1#MASM~BranchTestObjectShape, emit'1#CacheIR~GuardShape'2#MASM~BranchTestObjectShapeNoSpectreMitigations;
  
  emit'1#CacheIR~GuardShape'0#CacheIR~useObjectId'0#CacheIR~useTypedId'0#MASM~UnboxNonDouble:
  assume {:partition} (#MASM^emitPath == ConsEmitPath(ConsEmitPath(ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 0), 0), 0));
  op := #MASM^ops(#MASM^emitPath);
  call #MASM~UnboxNonDouble($valueReg##MASM^Op~UnboxNonDouble(op), $dstReg##MASM^Op~UnboxNonDouble(op), $valTy##MASM^Op~UnboxNonDouble(op));
  goto emit'1#CacheIR~GuardShape'1#MASM~BranchTestObjectShape, emit'1#CacheIR~GuardShape'2#MASM~BranchTestObjectShapeNoSpectreMitigations;
  
  emit'1#CacheIR~GuardShape'1#MASM~BranchTestObjectShape:
  assume {:partition} (#MASM^emitPath == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 1));
  op := #MASM^ops(#MASM^emitPath);
  call #MASM~BranchTestObjectShape($condition##MASM^Op~BranchTestObjectShape(op), $objectReg##MASM^Op~BranchTestObjectShape(op), $shape##MASM^Op~BranchTestObjectShape(op), $scratchReg##MASM^Op~BranchTestObjectShape(op), $spectreRegToZero##MASM^Op~BranchTestObjectShape(op), $branch##MASM^Op~BranchTestObjectShape(op));
  goto emit^Exit, emit'2#CacheIR~LoadFixedSlotResult'0#CacheIR~useObjectId'0#CacheIR~useTypedId'0#MASM~UnboxNonDouble, emit'2#CacheIR~LoadFixedSlotResult'1#MASM~LoadValueAddress;
  
  emit'1#CacheIR~GuardShape'2#MASM~BranchTestObjectShapeNoSpectreMitigations:
  assume {:partition} (#MASM^emitPath == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 2));
  op := #MASM^ops(#MASM^emitPath);
  call #MASM~BranchTestObjectShapeNoSpectreMitigations($condition##MASM^Op~BranchTestObjectShapeNoSpectreMitigations(op), $objectReg##MASM^Op~BranchTestObjectShapeNoSpectreMitigations(op), $shape##MASM^Op~BranchTestObjectShapeNoSpectreMitigations(op), $branch##MASM^Op~BranchTestObjectShapeNoSpectreMitigations(op));
  goto emit^Exit, emit'2#CacheIR~LoadFixedSlotResult'0#CacheIR~useObjectId'0#CacheIR~useTypedId'0#MASM~UnboxNonDouble, emit'2#CacheIR~LoadFixedSlotResult'1#MASM~LoadValueAddress;
  
  emit'2#CacheIR~LoadFixedSlotResult'0#CacheIR~useObjectId'0#CacheIR~useTypedId'0#MASM~UnboxNonDouble:
  assume {:partition} (#MASM^emitPath == ConsEmitPath(ConsEmitPath(ConsEmitPath(ConsEmitPath(NilEmitPath(), 2), 0), 0), 0));
  op := #MASM^ops(#MASM^emitPath);
  call #MASM~UnboxNonDouble($valueReg##MASM^Op~UnboxNonDouble(op), $dstReg##MASM^Op~UnboxNonDouble(op), $valTy##MASM^Op~UnboxNonDouble(op));
  goto emit'2#CacheIR~LoadFixedSlotResult'1#MASM~LoadValueAddress;
  
  emit'2#CacheIR~LoadFixedSlotResult'1#MASM~LoadValueAddress:
  assume {:partition} (#MASM^emitPath == ConsEmitPath(ConsEmitPath(NilEmitPath(), 2), 1));
  op := #MASM^ops(#MASM^emitPath);
  call #MASM~LoadValueAddress($address##MASM^Op~LoadValueAddress(op), $dstReg##MASM^Op~LoadValueAddress(op));
  goto emit^Exit;
  
  emit^Exit:
  assume {:partition} (#MASM^emitPath == NilEmitPath());
}
