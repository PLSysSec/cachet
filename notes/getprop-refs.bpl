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

type #NativeObject;

function #NativeObject^from#Object(in: #Object): #NativeObject;

function #NativeObject^to#Object(in: #NativeObject): #Object;

axiom (forall in: #NativeObject :: (#NativeObject^from#Object(#NativeObject^to#Object(in)) == in));

axiom (forall in: #Object :: (#NativeObject^to#Object(#NativeObject^from#Object(in)) == in));

type #ArgumentsObject;

function #ArgumentsObject^from#NativeObject(in: #NativeObject): #ArgumentsObject;

function #ArgumentsObject^to#NativeObject(in: #ArgumentsObject): #NativeObject;

axiom (forall in: #ArgumentsObject :: (#ArgumentsObject^from#NativeObject(#ArgumentsObject^to#NativeObject(in)) == in));

axiom (forall in: #NativeObject :: (#ArgumentsObject^to#NativeObject(#ArgumentsObject^from#NativeObject(in)) == in));

type #Shape;

type #Class;

type #Reg;

type #ValueId;

type #ObjectId;

type #Int32Field;

type #ShapeField;

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

procedure #Object~shapeOf($object: #Object)
  returns (ret: #Shape)
{
  var $tmp'v0: #Heap;
  
  $tmp'v0 := refs#Heap[#heap()];
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
  
  call out'0 := #Object~shapeOf($object);
  $shape'v0 := out'0;
  $class'v1 := #Shape~classOf($shape'v0);
  assert #Class~isNativeObject($class'v1);
  ret := #NativeObject^from#Object($object);
  return;
}

procedure #Object~toArgumentsObject($object: #Object)
  returns (ret: #ArgumentsObject)
{
  var $shape'v0: #Shape;
  var $class'v1: #Class;
  var out'0: #Shape;
  
  call out'0 := #Object~shapeOf($object);
  $shape'v0 := out'0;
  $class'v1 := #Shape~classOf($shape'v0);
  assert #Class~isArgumentsObject($class'v1);
  ret := #ArgumentsObject^from#NativeObject(#NativeObject^from#Object($object));
  return;
}

procedure #Object~getFixedSlot($object: #Object, $slot: #Int32)
  returns (ret: #Value)
{
  var $nativeObject'v0: #NativeObject;
  var out'0: #NativeObject;
  
  call out'0 := #Object~toNativeObject($object);
  $nativeObject'v0 := out'0;
  call ret := #NativeObject~getFixedSlot($nativeObject'v0, $slot);
  return;
}

procedure #NativeObject~getFixedSlot($nativeObject: #NativeObject, $slot: #Int32)
  returns (ret: #Value)
{
  var $shape'v0: #Shape;
  var $tmp'v1: #Heap;
  var out'0: #Shape;
  
  call out'0 := #Object~shapeOf(#NativeObject^to#Object($nativeObject));
  $shape'v0 := out'0;
  assert #Shape~hasFixedSlot($shape'v0, $slot);
  $tmp'v1 := refs#Heap[#heap()];
  ret := #NativeObject~getFixedSlotUnchecked($tmp'v1, $nativeObject, $slot);
  return;
}

function #NativeObject~getFixedSlotUnchecked($heap: #Heap, $nativeObject: #NativeObject, $slot: #Int32): #Value;

function #ArgumentsObject~getInitialLength($obj: #ArgumentsObject): #Int32;

function #ArgumentsObject~hasOverriddenLength($obj: #ArgumentsObject): #Bool;

function #Shape~classOf($shape: #Shape): #Class;

function #Shape~hasFixedSlot($shape: #Shape, $slot: #Int32): #Bool;

function #Class~isNativeObject($class: #Class): #Bool;

function #Class~isArgumentsObject($class: #Class): #Bool;

procedure {:inline 1} #CacheIR~addFailurePath($failure: #MASM^Label)
{
  call #MASM^bindExit($failure);
}

function #CacheIR~useValueReg($valueId: #ValueId): #ValueReg;

function #CacheIR~useObjectReg($objectId: #ObjectId): #Reg;

function #CacheIR~readInt32Field($int32Field: #Int32Field): #Int32;

function #CacheIR~readShapeField($shapeField: #ShapeField): #Shape;

function #CacheIR~objectGuardNeedsSpectreMitigations($objectId: #ObjectId): #Bool;

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

function {:constructor} #MASM^Op~BranchTestObjectShape($condition: #Condition, $objectReg: #Reg, $shape: #Shape, $scratchReg: #Reg, $spectreRegToZero: #Reg, $branch: #MASM^Label): #MASM^Op;

procedure #MASM~BranchTestObjectShape($condition: #Condition, $objectReg: #Reg, $shape: #Shape, $scratchReg: #Reg, $spectreRegToZero: #Reg, $branch: #MASM^Label)
{
  var $object'v0: #Object;
  var $objectHasShape'v1: #Bool;
  var $tmp'v2: #Shape;
  var $tmp'v3: #Int32;
  var $tmp'v4: #Int32;
  var out'0: #Object;
  var out'1: #Shape;
  var out'2: #Int32;
  var out'3: #Int32;
  
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~shapeOf($object'v0);
  $tmp'v2 := out'1;
  $objectHasShape'v1 := ($tmp'v2 == $shape);
  call #MASM~setInt32($scratchReg, 0bv32);
  if ($condition == #Condition^Variant~Equal()) {
    if ($objectHasShape'v1) {
      call out'2 := #MASM~getInt32($scratchReg);
      $tmp'v3 := out'2;
      call #MASM~setInt32($spectreRegToZero, $tmp'v3);
      call #MASM^goto($branch);
      return;
    }
  }
  if ($condition == #Condition^Variant~NotEqual()) {
    if (!$objectHasShape'v1) {
      call out'3 := #MASM~getInt32($scratchReg);
      $tmp'v4 := out'3;
      call #MASM~setInt32($spectreRegToZero, $tmp'v4);
      call #MASM^goto($branch);
      return;
    }
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~BranchTestObjectShapeNoSpectreMitigations($condition: #Condition, $objectReg: #Reg, $shape: #Shape, $branch: #MASM^Label): #MASM^Op;

procedure #MASM~BranchTestObjectShapeNoSpectreMitigations($condition: #Condition, $objectReg: #Reg, $shape: #Shape, $branch: #MASM^Label)
{
  var $object'v0: #Object;
  var $objectHasShape'v1: #Bool;
  var $tmp'v2: #Shape;
  var out'0: #Object;
  var out'1: #Shape;
  
  call out'0 := #MASM~getObject($objectReg);
  $object'v0 := out'0;
  call out'1 := #Object~shapeOf($object'v0);
  $tmp'v2 := out'1;
  $objectHasShape'v1 := ($tmp'v2 == $shape);
  if ($condition == #Condition^Variant~Equal()) {
    if ($objectHasShape'v1) {
      call #MASM^goto($branch);
      return;
    }
  }
  if ($condition == #Condition^Variant~NotEqual()) {
    if (!$objectHasShape'v1) {
      call #MASM^goto($branch);
      return;
    }
  }
  call #MASM^step();
  return;
}

function {:constructor} #MASM^Op~LoadObjectFixedSlot($objectReg: #Reg, $slot: #Int32, $outputReg: #ValueReg): #MASM^Op;

procedure #MASM~LoadObjectFixedSlot($objectReg: #Reg, $slot: #Int32, $outputReg: #ValueReg)
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

function {:constructor} #MASM^Op~LoadArgumentsObjectLength($objectReg: #Reg, $outputReg: #Reg, $failure: #MASM^Label): #MASM^Op;

procedure #MASM~LoadArgumentsObjectLength($objectReg: #Reg, $outputReg: #Reg, $failure: #MASM^Label)
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

function {:constructor} #MASM^Op~TagValue($valTy: #ValueType, $payload: #Reg, $dest: #ValueReg): #MASM^Op;

procedure #MASM~TagValue($valTy: #ValueType, $payload: #Reg, $dest: #ValueReg)
{
  var $i'v0: #Int32;
  var $val'v1: #Value;
  var $o'v2: #Object;
  var $val'v3: #Value;
  var out'0: #Int32;
  var out'1: #Value;
  var out'2: #Object;
  var out'3: #Value;
  
  if ($valTy == #ValueType^Variant~Int32()) {
    call out'0 := #MASM~getInt32($payload);
    $i'v0 := out'0;
    call out'1 := #Value~fromInt32($i'v0);
    $val'v1 := out'1;
    call #MASM~setValue($dest, $val'v1);
  } else if ($valTy == #ValueType^Variant~Object()) {
    call out'2 := #MASM~getObject($payload);
    $o'v2 := out'2;
    call out'3 := #Value~fromObject($o'v2);
    $val'v3 := out'3;
    call #MASM~setValue($dest, $val'v3);
  }
  call #MASM^step();
  return;
}

procedure #CacheIR~GuardIsNull($valueId: #ValueId, emitPath: EmitPath)
{
  var $valueReg'v0: #ValueReg;
  var $failure'l0: #MASM^Label;
  
  $valueReg'v0 := #CacheIR~useValueReg($valueId);
  call $failure'l0 := #MASM^label();
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~BranchTestNull(#Condition^Variant~NotEqual(), $valueReg'v0, $failure'l0), ConsEmitPath(emitPath, 0));
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

procedure #CacheIR~GuardShape($objectId: #ObjectId, $shapeField: #ShapeField, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $shape'v1: #Shape;
  var $needsSpectreMitigations'v2: #Bool;
  var $scratchReg'v3: #Reg;
  var $failure'l0: #MASM^Label;
  var out'0: #Reg;
  
  $objectReg'v0 := #CacheIR~useObjectReg($objectId);
  $shape'v1 := #CacheIR~readShapeField($shapeField);
  call $failure'l0 := #MASM^label();
  call #CacheIR~addFailurePath($failure'l0);
  $needsSpectreMitigations'v2 := #CacheIR~objectGuardNeedsSpectreMitigations($objectId);
  if ($needsSpectreMitigations'v2) {
    call out'0 := #CacheIR~allocateReg();
    $scratchReg'v3 := out'0;
    call #MASM^emit(#MASM^Op~BranchTestObjectShape(#Condition^Variant~NotEqual(), $objectReg'v0, $shape'v1, $scratchReg'v3, $objectReg'v0, $failure'l0), ConsEmitPath(emitPath, 0));
    call #CacheIR~releaseReg($scratchReg'v3);
  } else {
    call #MASM^emit(#MASM^Op~BranchTestObjectShapeNoSpectreMitigations(#Condition^Variant~NotEqual(), $objectReg'v0, $shape'v1, $failure'l0), ConsEmitPath(emitPath, 1));
  }
  return;
}

procedure #CacheIR~LoadFixedSlotResult($objectId: #ObjectId, $slotField: #Int32Field, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $slot'v1: #Int32;
  
  $objectReg'v0 := #CacheIR~useObjectReg($objectId);
  $slot'v1 := #CacheIR~readInt32Field($slotField);
  call #MASM^emit(#MASM^Op~LoadObjectFixedSlot($objectReg'v0, $slot'v1, #CacheIR~outputReg), ConsEmitPath(emitPath, 0));
  return;
}

procedure #CacheIR~LoadArgumentsObjectLengthResult($objectId: #ObjectId, emitPath: EmitPath)
{
  var $objectReg'v0: #Reg;
  var $argObjLengthReg'v1: #Reg;
  var $failure'l0: #MASM^Label;
  var out'0: #Reg;
  
  $objectReg'v0 := #CacheIR~useObjectReg($objectId);
  call out'0 := #CacheIR~allocateReg();
  $argObjLengthReg'v1 := out'0;
  call $failure'l0 := #MASM^label();
  call #CacheIR~addFailurePath($failure'l0);
  call #MASM^emit(#MASM^Op~LoadArgumentsObjectLength($objectReg'v0, $argObjLengthReg'v1, $failure'l0), ConsEmitPath(emitPath, 0));
  call #MASM^emit(#MASM^Op~TagValue(#ValueType^Variant~Int32(), $argObjLengthReg'v1, #CacheIR~outputReg), ConsEmitPath(emitPath, 1));
  call #CacheIR~releaseReg($argObjLengthReg'v1);
  return;
}

procedure #CacheStub~GetProp($valueId: #ValueId, $objectId: #ObjectId, $shapeField: #ShapeField, $slotField: #Int32Field, emitPath: EmitPath)
{
  var $shape'v0: #Shape;
  var $slot'v1: #Int32;
  var $tmp'v2: #ValueReg;
  var $tmp'v3: #ValueReg;
  var $tmp'v4: #Reg;
  var $tmp'v5: #Reg;
  var $tmp'v6: #Class;
  var out'0: #ValueReg;
  var out'1: #Reg;
  
  $tmp'v2 := #CacheIR~useValueReg($valueId);
  call out'0 := #CacheIR~allocateValueReg();
  $tmp'v3 := out'0;
  assume ($tmp'v2 == $tmp'v3);
  $tmp'v4 := #CacheIR~useObjectReg($objectId);
  call out'1 := #CacheIR~allocateReg();
  $tmp'v5 := out'1;
  assume ($tmp'v4 == $tmp'v5);
  $shape'v0 := #CacheIR~readShapeField($shapeField);
  $tmp'v6 := #Shape~classOf($shape'v0);
  assume #Class~isNativeObject($tmp'v6);
  assume #Shape~hasFixedSlot($shape'v0, 0bv32);
  assume #Shape~hasFixedSlot($shape'v0, 1bv32);
  assume #Shape~hasFixedSlot($shape'v0, 2bv32);
  $slot'v1 := #CacheIR~readInt32Field($slotField);
  assume ($slot'v1 == 1bv32);
  call #CacheIR~GuardToObject($valueId, $objectId, ConsEmitPath(emitPath, 0));
  call #CacheIR~GuardShape($objectId, $shapeField, ConsEmitPath(emitPath, 1));
  call #CacheIR~LoadFixedSlotResult($objectId, $slotField, ConsEmitPath(emitPath, 2));
  return;
}

procedure {:entrypoint} EntryPoint#CacheStub~GetProp($valueId: #ValueId, $objectId: #ObjectId, $shapeField: #ShapeField, $slotField: #Int32Field)
{
  var op: #MASM^Op;
  
  assume (forall pc: Pc :: (#MASM^pcEmitPaths[pc] == NilEmitPath()));
  #MASM^pc := 0;
  call #MASM^emit(#MASM^Op^Exit(), NilEmitPath());
  call #CacheStub~GetProp($valueId, $objectId, $shapeField, $slotField, NilEmitPath());
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
  goto emit'1#CacheIR~GuardShape'0#MASM~BranchTestObjectShape, emit'1#CacheIR~GuardShape'1#MASM~BranchTestObjectShapeNoSpectreMitigations;
  
  emit'1#CacheIR~GuardShape'0#MASM~BranchTestObjectShape:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 0));
  op := #MASM^ops[#MASM^pc];
  call #MASM~BranchTestObjectShape($condition##MASM^Op~BranchTestObjectShape(op), $objectReg##MASM^Op~BranchTestObjectShape(op), $shape##MASM^Op~BranchTestObjectShape(op), $scratchReg##MASM^Op~BranchTestObjectShape(op), $spectreRegToZero##MASM^Op~BranchTestObjectShape(op), $branch##MASM^Op~BranchTestObjectShape(op));
  goto emit^Exit, emit'2#CacheIR~LoadFixedSlotResult'0#MASM~LoadObjectFixedSlot;
  
  emit'1#CacheIR~GuardShape'1#MASM~BranchTestObjectShapeNoSpectreMitigations:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 1), 1));
  op := #MASM^ops[#MASM^pc];
  call #MASM~BranchTestObjectShapeNoSpectreMitigations($condition##MASM^Op~BranchTestObjectShapeNoSpectreMitigations(op), $objectReg##MASM^Op~BranchTestObjectShapeNoSpectreMitigations(op), $shape##MASM^Op~BranchTestObjectShapeNoSpectreMitigations(op), $branch##MASM^Op~BranchTestObjectShapeNoSpectreMitigations(op));
  goto emit^Exit, emit'2#CacheIR~LoadFixedSlotResult'0#MASM~LoadObjectFixedSlot;
  
  emit'2#CacheIR~LoadFixedSlotResult'0#MASM~LoadObjectFixedSlot:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == ConsEmitPath(ConsEmitPath(NilEmitPath(), 2), 0));
  op := #MASM^ops[#MASM^pc];
  call #MASM~LoadObjectFixedSlot($objectReg##MASM^Op~LoadObjectFixedSlot(op), $slot##MASM^Op~LoadObjectFixedSlot(op), $outputReg##MASM^Op~LoadObjectFixedSlot(op));
  goto emit^Exit;
  
  emit^Exit:
  assume {:partition} (#MASM^pcEmitPaths[#MASM^pc] == NilEmitPath());
}
