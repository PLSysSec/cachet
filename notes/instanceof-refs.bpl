type {:datatype} Ref#Map.#Reg.#Value;
function {:constructor} Local#Map.#Reg.#Value(frame: int, index: int): Ref#Map.#Reg.#Value;
var refs#Map.#Reg.#Value: [Ref#Map.#Reg.#Value]#Map #Reg #Value;

type {:datatype} Ref#Set.#Reg;
function {:constructor} Local#Set.#Reg(frame: int, index: int): Ref#Set.#Reg;
var refs#Set.#Reg: [Ref#Set.#Reg]#Set #Reg;

type {:datatype} Ref#Heap;
function {:constructor} Local#Heap(frame: int, index: int): Ref#Heap;
var refs#Heap: [Ref#Heap]#Heap;

type #ValueReg = #Reg;

function {:constructor} #MASM~regs(): Ref#Map.#Reg.#Value;
    
procedure #MASM~getValue($valueReg: #ValueReg)
  returns (ret: #Value)
{
  ret := #Map~get(refs#Map.#Reg.#Value[#MASM~regs()], $valueReg);
}

procedure #MASM~setValue($valueReg: #ValueReg, $value: #Value)
{
  refs#Map.#Reg.#Value[#MASM~regs()] := #Map~set(refs#Map.#Reg.#Value[#MASM~regs()], $valueReg, $value);
}

procedure #MASM~getInt32($reg: #Reg)
  returns (ret: #Int32)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getValue($reg);
  call ret := #Value~toInt32(tmp'0);
}

procedure #MASM~setInt32($reg: #Reg, $int32: #Int32)
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromInt32($int32);
  call #MASM~setValue($reg, tmp'0);
}

procedure #MASM~getBool($reg: #Reg)
  returns (ret: #Bool)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getValue($reg);
  call ret := #Value~toBool(tmp'0);
}

procedure #MASM~setBool($reg: #Reg, $bool: #Bool)
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromBool($bool);
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
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromObject($object);
  call #MASM~setValue($reg, tmp'0);
}

procedure #CacheIR~allocateValueReg()
  returns (ret: #ValueReg)
{
  call ret := #CacheIR~allocateReg();
}

procedure #CacheIR~releaseValueReg($valueReg: #ValueReg)
{
  call #CacheIR~releaseReg($valueReg);
}

function {:constructor} #CacheIR~allocatedRegs(): Ref#Set.#Reg;

procedure #CacheIR~allocateReg()
  returns (ret: #Reg)
{
  var $reg: #Reg;
  var tmp'0: #Bool;
  $reg := #CacheIR~allocateRegUnchecked(refs#Set.#Reg[#CacheIR~allocatedRegs()]);
  tmp'0 := #Set~contains(refs#Set.#Reg[#CacheIR~allocatedRegs()], $reg);
  assume !tmp'0;
  refs#Set.#Reg[#CacheIR~allocatedRegs()] := #Set~add(refs#Set.#Reg[#CacheIR~allocatedRegs()], $reg);
  ret := $reg;
}

function #CacheIR~allocateRegUnchecked($allocatedRegs: #Set #Reg): #Reg;

procedure #CacheIR~releaseReg($reg: #Reg)
{
  refs#Set.#Reg[#CacheIR~allocatedRegs()] := #Set~remove(refs#Set.#Reg[#CacheIR~allocatedRegs()], $reg);
}

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

function {:bvbuiltin "bvneg"} #Int32^negate(n: #Int32): #Int32;
function {:bvbuiltin "bvadd"} #Int32^add(x: #Int32, y: #Int32): #Int32;
function {:bvbuiltin "bvsle"} #Int32^lte(a: #Int32, y: #Int32): #Bool;
function {:bvbuiltin "bvsge"} #Int32^gte(a: #Int32, y: #Int32): #Bool;
function {:bvbuiltin "bvslt"} #Int32^lt(a: #Int32, y: #Int32): #Bool;
function {:bvbuiltin "bvsgt"} #Int32^gt(a: #Int32, y: #Int32): #Bool;

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

type {:datatype} #Condition;

function {:constructor} #Condition^Variant~Equal(): #Condition;

function {:constructor} #Condition^Variant~NotEqual(): #Condition;

type #Heap;

type #Value;

type #Object;

type #Reg;

type #ValueId;

type #ObjectId;

type {:datatype} #MASM^Op;

function {:constructor} #MASM^Op^Exit(): #MASM^Op;

var #MASM^pc: Pc;

var #MASM^ops: [Pc]#MASM^Op;

procedure {:inline 1} #MASM^step()
{
  #MASM^pc := (#MASM^pc + 1);
}

var #MASM^pcEmitPaths: [Pc]EmitPath;

procedure #MASM^emit(op: #MASM^Op, emitPath: EmitPath)
{
  #MASM^pcEmitPaths[#MASM^pc] := emitPath;
  #MASM^ops[#MASM^pc] := op;
  #MASM^pc := (#MASM^pc + 1);
}

type #MASM^Label = int;

var #MASM^nextLabel: #MASM^Label;

var #MASM^labelPcs: [#MASM^Label]Pc;

procedure #MASM^label()
  returns (ret: #MASM^Label)
{
  ret := #MASM^nextLabel;
  #MASM^nextLabel := (#MASM^nextLabel + 1);
}

procedure {:inline 1} #MASM^bind(label: #MASM^Label)
{
  #MASM^labelPcs[label] := #MASM^pc;
}

procedure {:inline 1} #MASM^bindExit(label: #MASM^Label)
{
  #MASM^labelPcs[label] := 0;
}

procedure {:inline 1} #MASM^goto(label: #MASM^Label)
{
  #MASM^pc := #MASM^labelPcs[label];
}

function {:constructor} #heap(): Ref#Heap;

const #zero: #Int32;

const #CacheIR~outputReg: #ValueReg;

function #Value~typeOf($value: #Value): #ValueType;

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

procedure #Value~isMagic($value: #Value)
  returns (ret: #Bool)
{
  var $tmp'v0: #ValueType;
  
  $tmp'v0 := #Value~typeOf($value);
  ret := ($tmp'v0 == #ValueType^Variant~Magic());
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

procedure #Object~protoOf($object: #Object)
  returns (ret: #Value)
{
  var $tmp'v0: #Heap;
  
  $tmp'v0 := refs#Heap[#heap()];
  ret := #Object~protoOfUnchecked($tmp'v0, $object);
  return;
}

function #Object~protoOfUnchecked($heap: #Heap, $object: #Object): #Value;

procedure {:inline 1} #CacheIR~addFailurePath($failure: #MASM^Label)
{
  call #MASM^bindExit($failure);
}

function #CacheIR~useValueReg($valueId: #ValueId): #ValueReg;

function #CacheIR~useObjectReg($objectId: #ObjectId): #Reg;

function {:constructor} #MASM^Op~Jump($dst: #MASM^Label): #MASM^Op;

procedure #MASM~Jump($dst: #MASM^Label)
{
  call #MASM^goto($dst);
  return;
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

function {:constructor} #MASM^Op~BranchTestObject($condition: #Condition, $valueReg: #ValueReg, $branch: #MASM^Label): #MASM^Op;

procedure #MASM~BranchTestObject($condition: #Condition, $valueReg: #ValueReg, $branch: #MASM^Label)
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

function {:constructor} #MASM^Op~BranchTestNull($condition: #Condition, $valueReg: #ValueReg, $branch: #MASM^Label): #MASM^Op;

procedure #MASM~BranchTestNull($condition: #Condition, $valueReg: #ValueReg, $branch: #MASM^Label)
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

function {:constructor} #MASM^Op~BranchTestMagic($condition: #Condition, $valueReg: #ValueReg, $branch: #MASM^Label): #MASM^Op;

procedure #MASM~BranchTestMagic($condition: #Condition, $valueReg: #ValueReg, $branch: #MASM^Label)
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

function {:constructor} #MASM^Op~BranchObject($condition: #Condition, $lhsReg: #Reg, $rhsReg: #Reg, $branch: #MASM^Label): #MASM^Op;

procedure #MASM~BranchObject($condition: #Condition, $lhsReg: #Reg, $rhsReg: #Reg, $branch: #MASM^Label)
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

function {:constructor} #MASM^Op~LoadObjectProto($objectReg: #Reg, $protoReg: #ValueReg): #MASM^Op;

procedure #MASM~LoadObjectProto($objectReg: #Reg, $protoReg: #ValueReg)
{
  var $object'v0: #Object;
  var $tmp'v1: #Value;
  var out'0: #Object;
  var out'1: #Value;
  
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~protoOf($object'v0);
  $tmp'v1 := out'1;
  call #MASM~setValue($protoReg, $tmp'v1);
  call #MASM^step();
  return;
}

procedure #CacheIR~GuardToObject($valueId: #ValueId, $objectId: #ObjectId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $objectReg'v1: #Reg;
  var $failure'l0: #MASM^Label;
  
  $valueReg'v0 := #CacheIR~useValueReg($valueId);
  $objectReg'v1 := #CacheIR~useObjectReg($objectId);
  call $failure'l0 := #MASM^label();
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestObject(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 0));
  call #MASM^emit(#MASM^Op~UnboxObject($valueReg'v0, $objectReg'v1), ConsEmitPath(emitPath, 1));
  return;
}

procedure #CacheIR~LoadInstanceOfObjectResult($lhsId: #ValueId, $protoId: #ObjectId, emitPath: EmitPath)
{
  var $lhsReg'v0: #ValueReg;
  var $protoReg'v1: #Reg;
  var $scratchReg'v2: #Reg;
  var $failure'l0: #MASM^Label;
  var $loopHead'l1: #MASM^Label;
  var $returnFalse'l2: #MASM^Label;
  var $returnTrue'l3: #MASM^Label;
  var $done'l4: #MASM^Label;
  var out'0: #Reg;
  
  $lhsReg'v0 := #CacheIR~useValueReg($lhsId);
  $protoReg'v1 := #CacheIR~useObjectReg($protoId);
  call out'0 := #CacheIR~allocateReg();
  $scratchReg'v2 := out'0;
  call $failure'l0 := #MASM^label();
  call #CacheIR~addFailurePath($failure'l0);
  call $loopHead'l1 := #MASM^label();
  call $returnFalse'l2 := #MASM^label();
  call $returnTrue'l3 := #MASM^label();
  call $done'l4 := #MASM^label();
  call #MASM^emit(#MASM^Op~BranchTestObject(#Condition^Variant~NotEqual(), $lhsReg'v0, $returnFalse'l2), ConsEmitPath(emitPath, 0));
  call #MASM^emit(#MASM^Op~UnboxObject($lhsReg'v0, $scratchReg'v2), ConsEmitPath(emitPath, 1));
  call #MASM^emit(#MASM^Op~LoadObjectProto($scratchReg'v2, $lhsReg'v0), ConsEmitPath(emitPath, 2));
  call #MASM^bind($loopHead'l1);
  call #MASM^emit(#MASM^Op~BranchTestNull(#Condition^Variant~Equal(), $lhsReg'v0, $returnFalse'l2), ConsEmitPath(emitPath, 3));
  call #MASM^emit(#MASM^Op~BranchTestMagic(#Condition^Variant~Equal(), $lhsReg'v0, $failure'l0), ConsEmitPath(emitPath, 4));
  call #MASM^emit(#MASM^Op~BranchTestObject(#Condition^Variant~NotEqual(), $lhsReg'v0, $returnFalse'l2), ConsEmitPath(emitPath, 5));
  call #MASM^emit(#MASM^Op~UnboxObject($lhsReg'v0, $scratchReg'v2), ConsEmitPath(emitPath, 6));
  call #MASM^emit(#MASM^Op~BranchObject(#Condition^Variant~Equal(), $scratchReg'v2, $protoReg'v1, $returnTrue'l3), ConsEmitPath(emitPath, 7));
  call #MASM^emit(#MASM^Op~LoadObjectProto($scratchReg'v2, $lhsReg'v0), ConsEmitPath(emitPath, 8));
  call #MASM^emit(#MASM^Op~Jump($loopHead'l1), ConsEmitPath(emitPath, 9));
  call #MASM^bind($returnFalse'l2);
  call #MASM^emit(#MASM^Op~StoreBoolValue(false, #CacheIR~outputReg), ConsEmitPath(emitPath, 10));
  call #MASM^emit(#MASM^Op~Jump($done'l4), ConsEmitPath(emitPath, 11));
  call #MASM^bind($returnTrue'l3);
  call #MASM^emit(#MASM^Op~StoreBoolValue(true, #CacheIR~outputReg), ConsEmitPath(emitPath, 12));
  call #MASM^bind($done'l4);
  call #CacheIR~releaseReg($scratchReg'v2);
  return;
}

procedure #CacheStub~InstanceOf($lhsId: #ValueId, $rhsId: #ValueId, $protoId: #ObjectId, emitPath: EmitPath)
{
  var $tmp'v0: #ValueReg;
  var $tmp'v1: #ValueReg;
  var $tmp'v2: #ValueReg;
  var $tmp'v3: #ValueReg;
  var $tmp'v4: #Reg;
  var $tmp'v5: #Reg;
  var out'0: #ValueReg;
  var out'1: #ValueReg;
  var out'2: #Reg;
  
  $tmp'v0 := #CacheIR~useValueReg($lhsId);
  call out'0 := #CacheIR~allocateValueReg();
  $tmp'v1 := out'0;
  assume ($tmp'v0 == $tmp'v1);
  $tmp'v2 := #CacheIR~useValueReg($rhsId);
  call out'1 := #CacheIR~allocateValueReg();
  $tmp'v3 := out'1;
  assume ($tmp'v2 == $tmp'v3);
  $tmp'v4 := #CacheIR~useObjectReg($protoId);
  call out'2 := #CacheIR~allocateReg();
  $tmp'v5 := out'2;
  assume ($tmp'v4 == $tmp'v5);
  call #CacheIR~GuardToObject($rhsId, $protoId, ConsEmitPath(emitPath, 0));
  call #CacheIR~LoadInstanceOfObjectResult($lhsId, $protoId, ConsEmitPath(emitPath, 1));
  return;
}

procedure {:entrypoint} EntryPoint#CacheStub~InstanceOf($lhsId: #ValueId, $rhsId: #ValueId, $protoId: #ObjectId)
{
  var op: #MASM^Op;
  
  assume (forall pc: Pc :: (#MASM^pcEmitPaths[pc] == NilEmitPath()));
  #MASM^pc := 0;
  call #MASM^emit(#MASM^Op^Exit(), NilEmitPath());
  call #CacheStub~InstanceOf($lhsId, $rhsId, $protoId, NilEmitPath());
  call #MASM^emit(#MASM^Op^Exit(), NilEmitPath());
  #MASM^pc := 1;
  
  emit'0#CacheIR~GuardToObject'0#MASM~BranchTestObject:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 0), 0));
  op := #MASM^ops[#MASM^pc];
  call #MASM~BranchTestObject($condition##MASM^Op~BranchTestObject(op), $valueReg##MASM^Op~BranchTestObject(op), $branch##MASM^Op~BranchTestObject(op));
  goto emit^Exit, emit'0#CacheIR~GuardToObject'1#MASM~UnboxObject;
  
  emit'0#CacheIR~GuardToObject'1#MASM~UnboxObject:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 0), 1));
  op := #MASM^ops[#MASM^pc];
  call #MASM~UnboxObject($valueReg##MASM^Op~UnboxObject(op), $objectReg##MASM^Op~UnboxObject(op));
  goto emit'1#CacheIR~LoadInstanceOfObjectResult'0#MASM~BranchTestObject;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'0#MASM~BranchTestObject:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 0));
  op := #MASM^ops[#MASM^pc];
  call #MASM~BranchTestObject($condition##MASM^Op~BranchTestObject(op), $valueReg##MASM^Op~BranchTestObject(op), $branch##MASM^Op~BranchTestObject(op));
  goto emit'1#CacheIR~LoadInstanceOfObjectResult'1#MASM~UnboxObject, emit'1#CacheIR~LoadInstanceOfObjectResult'10#MASM~StoreBoolValue;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'1#MASM~UnboxObject:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 1));
  op := #MASM^ops[#MASM^pc];
  call #MASM~UnboxObject($valueReg##MASM^Op~UnboxObject(op), $objectReg##MASM^Op~UnboxObject(op));
  goto emit'1#CacheIR~LoadInstanceOfObjectResult'2#MASM~LoadObjectProto;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'2#MASM~LoadObjectProto:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 2));
  op := #MASM^ops[#MASM^pc];
  call #MASM~LoadObjectProto($objectReg##MASM^Op~LoadObjectProto(op), $protoReg##MASM^Op~LoadObjectProto(op));
  goto emit'1#CacheIR~LoadInstanceOfObjectResult'3#MASM~BranchTestNull;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'3#MASM~BranchTestNull:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 3));
  op := #MASM^ops[#MASM^pc];
  call #MASM~BranchTestNull($condition##MASM^Op~BranchTestNull(op), $valueReg##MASM^Op~BranchTestNull(op), $branch##MASM^Op~BranchTestNull(op));
  goto emit'1#CacheIR~LoadInstanceOfObjectResult'4#MASM~BranchTestMagic, emit'1#CacheIR~LoadInstanceOfObjectResult'10#MASM~StoreBoolValue;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'4#MASM~BranchTestMagic:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 4));
  op := #MASM^ops[#MASM^pc];
  call #MASM~BranchTestMagic($condition##MASM^Op~BranchTestMagic(op), $valueReg##MASM^Op~BranchTestMagic(op), $branch##MASM^Op~BranchTestMagic(op));
  goto emit^Exit, emit'1#CacheIR~LoadInstanceOfObjectResult'5#MASM~BranchTestObject;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'5#MASM~BranchTestObject:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 5));
  op := #MASM^ops[#MASM^pc];
  call #MASM~BranchTestObject($condition##MASM^Op~BranchTestObject(op), $valueReg##MASM^Op~BranchTestObject(op), $branch##MASM^Op~BranchTestObject(op));
  goto emit'1#CacheIR~LoadInstanceOfObjectResult'6#MASM~UnboxObject, emit'1#CacheIR~LoadInstanceOfObjectResult'10#MASM~StoreBoolValue;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'6#MASM~UnboxObject:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 6));
  op := #MASM^ops[#MASM^pc];
  call #MASM~UnboxObject($valueReg##MASM^Op~UnboxObject(op), $objectReg##MASM^Op~UnboxObject(op));
  goto emit'1#CacheIR~LoadInstanceOfObjectResult'7#MASM~BranchObject;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'7#MASM~BranchObject:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 7));
  op := #MASM^ops[#MASM^pc];
  call #MASM~BranchObject($condition##MASM^Op~BranchObject(op), $lhsReg##MASM^Op~BranchObject(op), $rhsReg##MASM^Op~BranchObject(op), $branch##MASM^Op~BranchObject(op));
  goto emit'1#CacheIR~LoadInstanceOfObjectResult'8#MASM~LoadObjectProto, emit'1#CacheIR~LoadInstanceOfObjectResult'12#MASM~StoreBoolValue;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'8#MASM~LoadObjectProto:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 8));
  op := #MASM^ops[#MASM^pc];
  call #MASM~LoadObjectProto($objectReg##MASM^Op~LoadObjectProto(op), $protoReg##MASM^Op~LoadObjectProto(op));
  goto emit'1#CacheIR~LoadInstanceOfObjectResult'9#MASM~Jump;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'9#MASM~Jump:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 9));
  op := #MASM^ops[#MASM^pc];
  call #MASM~Jump($dst##MASM^Op~Jump(op));
  goto emit'1#CacheIR~LoadInstanceOfObjectResult'3#MASM~BranchTestNull;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'10#MASM~StoreBoolValue:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 10));
  op := #MASM^ops[#MASM^pc];
  call #MASM~StoreBoolValue($bool##MASM^Op~StoreBoolValue(op), $valueReg##MASM^Op~StoreBoolValue(op));
  goto emit'1#CacheIR~LoadInstanceOfObjectResult'11#MASM~Jump;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'11#MASM~Jump:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 11));
  op := #MASM^ops[#MASM^pc];
  call #MASM~Jump($dst##MASM^Op~Jump(op));
  goto emit^Exit;
  
  emit'1#CacheIR~LoadInstanceOfObjectResult'12#MASM~StoreBoolValue:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 12));
  op := #MASM^ops[#MASM^pc];
  call #MASM~StoreBoolValue($bool##MASM^Op~StoreBoolValue(op), $valueReg##MASM^Op~StoreBoolValue(op));
  goto emit^Exit;
  
  emit^Exit:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == NilEmitPath());
}
