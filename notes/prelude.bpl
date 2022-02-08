// ... begin prelude ...

type Pc = int;

type {:datatype} EmitPath;
function {:constructor} NilEmitPath(): EmitPath;
function {:constructor} ConsEmitPath(init: EmitPath, last: int): EmitPath;

type #Unit;

const #unit: #Unit;

axiom (forall x: #Unit, y: #Unit :: x == y);

type #Bool = bool;

type #Int32 = bv32;

function {:bvbuiltin "bvadd"} #Int32^Add(x: #Int32, y: #Int32): #Int32;
function {:bvbuiltin "bvneg"} #Int32^Negate(n: #Int32): #Int32;
// etc; see https://boogie-docs.readthedocs.io/en/latest/LangRef.html#other-operators

type #Double = float53e11; // 64-bit; see https://github.com/boogie-org/boogie/issues/29#issuecomment-231239065

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

// ... end prelude ...

type #ValueReg = #Reg;

var #MASM~regs: #Map #Reg #Value;
    
procedure #MASM~getValue($valueReg: #ValueReg)
  returns (ret: #Value)
{
  ret := #Map~get(#MASM~regs, $valueReg);
}

procedure #MASM~setValue($valueReg: #ValueReg, $value: #Value)
  modifies #MASM~regs;
{
  #MASM~regs := #Map~set(#MASM~regs, $valueReg, $value);
}

procedure #MASM~getInt32($reg: #Reg)
  returns (ret: #Int32)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getValue($reg);
  call ret := #Value~toInt32(tmp'0);
}

procedure #MASM~setInt32($reg: #Reg, $int32: #Int32)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromInt32($int32);
  call #MASM~setValue($reg, tmp'0);
}

procedure #MASM~getObject($reg: #Reg)
  returns (ret: #Object)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getValue($reg);
  call ret := #Value~toObject(tmp'0);
}

procedure #MASM~setObject($reg: #Reg, $object: #Object)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromObject($object);
  call #MASM~setValue($reg, tmp'0);
}

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

var #CacheIR~allocatedRegs: #Set #Reg;

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

function #CacheIR~allocateRegUnchecked($allocatedRegs: #Set #Reg): #Reg;

procedure #CacheIR~releaseReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  #CacheIR~allocatedRegs := #Set~remove(#CacheIR~allocatedRegs, $reg);
}

