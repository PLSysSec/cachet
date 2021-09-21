// ... begin prelude ...

type $Unit;

const $unit: $Unit;

axiom (forall x: $Unit, y: $Unit :: x == y);

type $Bool = bool;

type $Int32 = bv32;

function {:bvbuiltin "bvadd"} $Int32^Add(x: $Int32, y: $Int32): $Int32;
// etc; see https://boogie-docs.readthedocs.io/en/latest/LangRef.html#other-operators

type $Double = float53e11; // 64-bit; see https://github.com/boogie-org/boogie/issues/29#issuecomment-231239065

type $Map k v = [k]v;

function {:inline} $Map~get<k, v>(map: $Map k v, key: k): v {
  map[key]
}

function {:inline} $Map~set<k, v>(map: $Map k v, key: k, value: v): $Map k v {
  map[key := value]
}

type $Set a = $Map a $Bool;

function {:inline} $Set~contains<a>(set: $Set a, value: a): $Bool {
  $Map~get(set, value)
}

function {:inline} $Set~add<a>(set: $Set a, value: a): $Set a {
  $Map~set(set, value, true)
}

function {:inline} $Set~remove<a>(set: $Set a, value: a): $Set a {
  $Map~set(set, value, false)
}

// ... end prelude ...

// struct Heap;
type $Heap;
// var mut heap: Heap;
var $heap: $Heap;

// enum ValueType {
type {:datatype} $ValueType;
// Double,
function {:constructor} $ValueType~Double(): $ValueType;
// Int32,
function {:constructor} $ValueType~Int32(): $ValueType;
// Bool, 
function {:constructor} $ValueType~Bool(): $ValueType;
// Undefined,
function {:constructor} $ValueType~Undefined(): $ValueType;
// Null,
function {:constructor} $ValueType~Null(): $ValueType;
// Magic,
function {:constructor} $ValueType~Magic(): $ValueType;
// String, 
function {:constructor} $ValueType~String(): $ValueType;
// Symbol,
function {:constructor} $ValueType~Symbol(): $ValueType;
// PrivateGCThing,
function {:constructor} $ValueType~PrivateGCThing(): $ValueType;
// BigInt, 
function {:constructor} $ValueType~BigInt(): $ValueType;
// Object,
function {:constructor} $ValueType~Object(): $ValueType;
// }

// struct Value;
type $Value;

// impl Value {

// fn typeOf(value: Value) -> ValueType;
function $Value~typeOf($value: $Value): $ValueType;

// fn isObject(value: Value) -> Bool {
procedure $Value~isObject($value: $Value)
  returns (ret: $Bool)
{
  var tmp'0: $ValueType;
  // [0] <- Value::typeOf(value)
  tmp'0 := $Value~typeOf($value);
  // [0] == ValueType::Object
  ret := tmp'0 == $ValueType~Object();
// }
}

// fn fromObject(object: Object) -> Value {
procedure $Value~fromObject($object: $Object)
  returns (ret: $Value)
{
  var $value: $Value;
  var tmp'0: $Bool;
  var tmp'1: $Object;
  // let value = unsafe { Value::fromObjectUnchecked(object) };
  $value := $Value~fromObjectUnchecked($object);
  // [0] <- Value::isObject(value)
  call tmp'0 := $Value~isObject($value);
  // assume [0];
  assume tmp'0;
  // [1] <- unsafe { Value::toObjectUnchecked(value) }
  tmp'1 := $Value~toObjectUnchecked($value);
  // assume [1] == object;
  assume tmp'1 == $object;
  // value
  ret := $value;
// }
}

// unsafe fn fromObjectUnchecked(value: Object) -> Value;
function $Value~fromObjectUnchecked($object: $Object): $Value;

// fn toObject(value: Value) -> Object {
procedure $Value~toObject($value: $Value)
  returns (ret: $Object)
{
  var $object: $Object;
  var tmp'0: $Bool;
  var tmp'1: $Value;
  // [0] <- Value::isObject(value)
  call tmp'0 := $Value~isObject($value);
  // assert [0];
  assert tmp'0;
  // let object = unsafe { Value::toObjectUnchecked(value) };
  $object := $Value~toObjectUnchecked($value);
  // [1] <- unsafe { Value::fromObjectUnchecked(object) }
  tmp'1 := $Value~fromObjectUnchecked($object);
  // assume [1] == value;
  assume tmp'1 == $value;
  // object
  ret := $object;
// }
}

// unsafe fn toObjectUnchecked(value: Value) -> Object;
function $Value~toObjectUnchecked($value: $Value): $Object;

// }

// struct Object;
type $Object;

// impl Object {

// fn shapeOf(object: Object) -> Shape {
procedure $Object~shapeOf($object: $Object)
  returns (ret: $Shape)
{
  // unsafe { Object::shapeOfUnchecked(heap, object) }
  ret := $Object~shapeOfUnchecked($heap, $object);
// }
}

// unsafe fn shapeOfUnchecked(heap: Heap, object: Object) -> Shape;
function $Object~shapeOfUnchecked($heap: $Heap, $object: $Object): $Shape;

// fn toNativeObject(object: Object) -> NativeObject {
procedure $Object~toNativeObject($object: $Object)
  returns (ret: $NativeObject)
{
  var $shape: $Shape;
  var $class: $Class;
  var tmp'0: $Bool;
  // let shape = Object::shapeOf(object);
  call $shape := $Object~shapeOf($object);
  // let class = Shape::classOf(shape);
  $class := $Shape~classOf($shape);
  // [0] <- Class::isNativeObject(class) 
  tmp'0 := $Class~isNativeObject($class);
  // assert [0];
  assert tmp'0;
  // unsafe { object as NativeObject }
  ret := $NativeObject^from$Object($object);
// }
}

// fn getFixedSlot(object: Object, slot: Int32) -> Value {
procedure $Object~getFixedSlot($object: $Object, $slot: $Int32)
  returns (ret: $Value)
{
  var $nativeObject: $NativeObject;
  // let nativeObject = Object::toNativeObject(object);
  call $nativeObject := $Object~toNativeObject($object);
  // NativeObject::getFixedSlot(nativeObject, slot)
  call ret := $NativeObject~getFixedSlot($nativeObject, $slot);
// }
}

// }

// struct NativeObject <: Object;
type $NativeObject;

function $NativeObject^from$Object(in: $Object): $NativeObject;
function $NativeObject^to$Object(in: $NativeObject): $Object;
axiom (forall in: $NativeObject :: $NativeObject^from$Object($NativeObject^to$Object(in)) == in);
axiom (forall in: $Object :: $NativeObject^to$Object($NativeObject^from$Object(in)) == in);

// impl NativeObject {

// fn getFixedSlot(nativeObject: NativeObject, slot: Int32) -> Value {
procedure $NativeObject~getFixedSlot($nativeObject: $NativeObject, $slot: $Int32)
  returns (ret: $Value)
{
  var $shape: $Shape;
  var tmp'0: $Bool;
  // let shape = Object::shapeOf(nativeObject);
  call $shape := $Object~shapeOf($NativeObject^to$Object($nativeObject));
  // [0] <- Shape::hasFixedSlot(shape, slot)
  tmp'0 := $Shape~hasFixedSlot($shape, $slot);
  // assert [0];
  assert tmp'0;
  // unsafe { NativeObject::getFixedSlotUnchecked(heap, nativeObject, slot) }
  ret := $NativeObject~getFixedSlotUnchecked($heap, $nativeObject, $slot);
// }
}

// unsafe fn getFixedSlotUnchecked(heap: Heap, nativeObject: NativeObject, slot: Int32) -> Value;
function $NativeObject~getFixedSlotUnchecked(
  $heap: $Heap,
  $nativeObject: $NativeObject,
  $slot: $Int32
): $Value;

// }

// struct Shape;
type $Shape;

// impl Shape {

// fn classOf(shape: Shape) -> Class;
function $Shape~classOf($shape: $Shape): $Class;

// fn hasFixedSlot(shape: Shape, slot: Int32) -> Bool;
function $Shape~hasFixedSlot($shape: $Shape, $slot: $Int32): $Bool;

// }

// struct Class;
type $Class;

// impl Class {

// fn isNativeObject(class: Class) -> Bool;
function $Class~isNativeObject($class: $Class): $Bool;

// }

// type ValueReg = Reg;
type $ValueReg = $Reg;

// struct Reg;
type $Reg;

// ir MASM {

type {:datatype} $MASM^Op;

function {:constructor} $MASM^Op^External(): $MASM^Op;

type $MASM^Pc = int;

var $MASM^pc: $MASM^Pc;
var $MASM^ops: [$MASM^Pc]$MASM^Op;

procedure {:inline 1} $MASM^step()
  modifies $MASM^pc;
{
  $MASM^pc := $MASM^pc + 1;
}

type $MASM^EmitId = int;

var $MASM^pcEmitIds: [$MASM^Pc]$MASM^EmitId;

procedure $MASM^emit(emitId: $MASM^EmitId, op: $MASM^Op)
  modifies $MASM^pc, $MASM^ops, $MASM^pcEmitIds;
{
  $MASM^pcEmitIds[$MASM^pc] := emitId;
  $MASM^ops[$MASM^pc] := op;
  $MASM^pc := $MASM^pc + 1;
}

type $MASM^Label = int;

var $MASM^nextLabel: $MASM^Label;
var $MASM^labelPcs: [$MASM^Label]$MASM^Pc;

procedure $MASM^label()
  returns (label: $MASM^Label)
  modifies $MASM^nextLabel;
{
  label := $MASM^nextLabel;
  $MASM^nextLabel := $MASM^nextLabel + 1;
}

procedure {:inline 1} $MASM^bind(label: $MASM^Label)
  modifies $MASM^labelPcs;
{
  $MASM^labelPcs[label] := $MASM^pc;
}

procedure {:inline 1} $MASM^goto(label: $MASM^Label)
  modifies $MASM^pc;
{
  $MASM^pc := $MASM^labelPcs[label];
}

// var mut regs: Map<Reg, Value>;
var $MASM~regs: $Map $Reg $Value;

// fn getValue(valueReg: ValueReg) -> Value {
procedure $MASM~getValue($valueReg: $ValueReg)
  returns (ret: $Value)
{
  // Map::get(MASM::regs, valueReg)
  ret := $Map~get($MASM~regs, $valueReg);
// }
}

// fn setValue(valueReg: ValueReg, value: Value) {
procedure $MASM~setValue($valueReg: $ValueReg, $value: $Value)
  modifies $MASM~regs;
{
  // MASM::regs = Map::set(MASM::regs, valueReg, value);
  $MASM~regs := $Map~set($MASM~regs, $valueReg, $value);
// }
}

// fn getObject(reg: Reg) -> Object {
procedure $MASM~getObject($reg: $Reg)
  returns (ret: $Object)
{
  var tmp'0: $Value;
  // [0] <- Map::get(MASM::regs, reg)
  call tmp'0 := $MASM~getValue($reg);
  // Value::toObject([0])
  call ret := $Value~toObject(tmp'0);
// }
}

// fn setObject(reg: Reg, object: Object) {
procedure $MASM~setObject($reg: $Reg, $object: $Object)
  modifies $MASM~regs;
{
  var tmp'0: $Value;
  // [0] <- Value::fromObject(object)
  call tmp'0 := $Value~fromObject($object);
  // MASM::setValue(reg, [0]);
  call $MASM~setValue($reg, tmp'0);
}

// op BranchTestNotObject(valueReg: ValueReg, label branch) {
function {:constructor} $MASM^Op~BranchTestNotObject($valueReg: $ValueReg, $branch: $MASM^Label):
  $MASM^Op;

procedure $MASM~BranchTestNotObject($valueReg: $ValueReg, $branch: $MASM^Label)
  modifies $MASM^pc;
{
  var $value: $Value;
  var tmp'0: $Bool;
  // let value = MASM::getValue(valueReg);
  call $value := $MASM~getValue($valueReg);
  // [0] <- Value::isObject(value)
  call tmp'0 := $Value~isObject($value);
  // if ![0] {
  if (!tmp'0) {
    // goto branch;
    call $MASM^goto($branch);
    return;
  // }
  }
  call $MASM^step();
// }
}

// op UnboxObject(valueReg: ValueReg, objectReg: Reg)
function {:constructor} $MASM^Op~UnboxObject($valueReg: $ValueReg, $objectReg: $Reg): $MASM^Op;

procedure $MASM~UnboxObject($valueReg: $ValueReg, $objectReg: $Reg)
  modifies $MASM^pc, $MASM~regs;
{
  var $value: $Value;
  var tmp'0: $Object;
  // let value = MASM::getValue(valueReg);
  call $value := $MASM~getValue($valueReg);
  // [0] <- Value::toObject(value)
  call tmp'0 := $Value~toObject($value);
  // MASM::setObject(objectReg, [0]);
  call $MASM~setObject($objectReg, tmp'0);
  call $MASM^step();
// }
}

// op BranchTestNotObjectShape(objectReg: Reg, shape: Shape, label branch) {
function {:constructor} $MASM^Op~BranchTestNotObjectShape(
  $objectReg: $Reg,
  $shape: $Shape,
  $branch: $MASM^Label
): $MASM^Op;

procedure $MASM~BranchTestNotObjectShape(
  $objectReg: $Reg,
  $shape: $Shape,
  $branch: $MASM^Label
)
  modifies $MASM^pc;
{
  var $object: $Object;
  var tmp'0: $Shape;
  // let object = MASM::getObject(objectReg);
  call $object := $MASM~getObject($objectReg);
  // [0] <- Object::shapeOf(object)
  call tmp'0 := $Object~shapeOf($object);
  // if [0] != shape {
  if (tmp'0 != $shape) {
    // goto branch;
    call $MASM^goto($branch);
    return;
  // }
  }
  call $MASM^step();
// }
}

// op LoadObjectFixedSlot(objectReg: Reg, slot: Int32, outputReg: ValueReg) {
function {:constructor} $MASM^Op~LoadObjectFixedSlot(
  $objectReg: $Reg,
  $slot: $Int32,
  $outputReg: $ValueReg
): $MASM^Op;

procedure $MASM~LoadObjectFixedSlot(
  $objectReg: $Reg,
  $slot: $Int32,
  $outputReg: $ValueReg
)
  modifies $MASM^pc, $MASM~regs;
{
  var $object: $Object;
  var tmp'0: $Value;
  // let object = MASM::getObject(objectReg);
  call $object := $MASM~getObject($objectReg);
  // [0] <- Object::getFixedSlot(object, slot)
  call tmp'0 := $Object~getFixedSlot($object, $slot);
  // MASM::setValue(outputReg, [0]);
  call $MASM~setValue($outputReg, tmp'0);
  call $MASM^step();
// }
}

// }

// struct ValueId;
type $ValueId;

// struct ObjectId;
type $ObjectId;

// struct Int32Field;
type $Int32Field;

// struct ShapeField;
type $ShapeField;

// struct RegAlloc;
type $RegAlloc;

// ir CacheIR emits MASM {

// var mut regAlloc: RegAlloc;
var $CacheIR~regAlloc: $RegAlloc;

// fn allocateValueReg() -> ValueReg {
procedure $CacheIR~allocateValueReg()
  returns (ret: $ValueReg)
  modifies $CacheIR~regAlloc;
{
  var tmp'0: $CacheIR~allocateValueRegUnchecked^Ret;
  // unsafe { CacheIR::allocateValueRegUnchecked(CacheIR::regAlloc, out CacheIR::regAlloc) }
  tmp'0 := $CacheIR~allocateValueRegUnchecked($CacheIR~regAlloc);
  ret := $ret#$CacheIR~allocateValueRegUnchecked^ret(tmp'0);
  $CacheIR~regAlloc := $regAllocAfter#$CacheIR~allocateValueRegUnchecked^ret(tmp'0);
// }
}

// unsafe fn allocateValueRegUnchecked(regAlloc: RegAlloc, out regAllocAfter: RegAlloc)
    // -> ValueReg;

type {:datatype} $CacheIR~allocateValueRegUnchecked^Ret;

function {:constructor} $CacheIR~allocateValueRegUnchecked^ret(
  $ret: $ValueReg,
  $regAllocAfter: $RegAlloc
): $CacheIR~allocateValueRegUnchecked^Ret;

function $CacheIR~allocateValueRegUnchecked($regAlloc: $RegAlloc):
  $CacheIR~allocateValueRegUnchecked^Ret;

// fn releaseValueReg(valueReg: ValueReg) {
procedure $CacheIR~releaseValueReg($valueReg: $ValueReg)
  modifies $CacheIR~regAlloc;
{
  // unsafe {
    // CacheIR::releaseValueRegUnchecked(CacheIR::regAlloc, valueReg, out CacheIR::regAlloc)
  // }
  $CacheIR~regAlloc := $CacheIR~releaseValueRegUnchecked($CacheIR~regAlloc, $valueReg);
// }
}

// unsafe fn releaseValueRegUnchecked(
  // regAlloc: RegAlloc,
  // valueReg: ValueReg,
  // out regAllocAfter: RegAlloc
// );
function $CacheIR~releaseValueRegUnchecked($regAlloc: $RegAlloc, $valueReg: $ValueReg): $RegAlloc;

// fn allocateReg() -> Reg {
procedure $CacheIR~allocateReg()
  returns (ret: $Reg)
  modifies $CacheIR~regAlloc;
{
  var tmp'0: $CacheIR~allocateRegUnchecked^Ret;
  // unsafe { CacheIR::allocateRegUnchecked(CacheIR::regAlloc, out CacheIR::regAlloc) }
  tmp'0 := $CacheIR~allocateRegUnchecked($CacheIR~regAlloc);
  ret := $ret#$CacheIR~allocateRegUnchecked^ret(tmp'0);
  $CacheIR~regAlloc := $regAllocAfter#$CacheIR~allocateRegUnchecked^ret(tmp'0);
// }
}

// unsafe fn allocateRegUnchecked(regAlloc: RegAlloc, out regAllocAfter: RegAlloc) -> Reg;

type {:datatype} $CacheIR~allocateRegUnchecked^Ret;

function {:constructor} $CacheIR~allocateRegUnchecked^ret($ret: $Reg, $regAllocAfter: $RegAlloc):
  $CacheIR~allocateRegUnchecked^Ret;

function $CacheIR~allocateRegUnchecked($regAlloc: $RegAlloc): $CacheIR~allocateRegUnchecked^Ret;

// fn releaseReg(reg: Reg) {
procedure $CacheIR~releaseReg($reg: $Reg)
  modifies $CacheIR~regAlloc;
{
  // unsafe { CacheIR::releaseRegUnchecked(CacheIR::regAlloc, reg, out CacheIR::regAlloc) }
  $CacheIR~regAlloc := $CacheIR~releaseRegUnchecked($CacheIR~regAlloc, $reg);
// }
}

// unsafe fn releaseRegUnchecked(regAlloc: RegAlloc, reg: Reg, out regAllocAfter: RegAlloc);
function $CacheIR~releaseRegUnchecked($regAlloc: $RegAlloc, $reg: $Reg): $RegAlloc;

// fn useValueReg(valueId: ValueId) -> ValueReg;
function $CacheIR~useValueReg($valueId: $ValueId): $ValueReg;

// fn useObjectReg(objectId: ObjectId) -> Reg;
function $CacheIR~useObjectReg($objectId: $ObjectId): $Reg;

// var outputReg: ValueReg;
const $CacheIR~outputReg: $ValueReg;

// fn readInt32Field(int32Field: Int32Field) -> Int32;
function $CacheIR~readInt32Field($int32Field: $Int32Field): $Int32;

// fn readShapeField(shapeField: ShapeField) -> Shape;
function $CacheIR~readShapeField($shapeField: $ShapeField): $Shape;

// op GuardToObject(valueId: ValueId, objectId: ObjectId, label failure) {
procedure $CacheIR~GuardToObject($valueId: $ValueId, $objectId: $ObjectId, $failure: $MASM^Label)
  modifies
    $MASM^pc,
    $MASM^ops,
    $MASM^pcEmitIds;
{
  var $valueReg: $ValueReg;
  var $objectReg: $Reg;

  // let valueReg = CacheIR::useValueReg(valueId);
  $valueReg := $CacheIR~useValueReg($valueId);
  // let objectReg = CacheIR::useObjectReg(objectId);
  $objectReg := $CacheIR~useObjectReg($objectId);

  // emit MASM::BranchTestNotObject(valueReg, failure);
  call $MASM^emit(0, $MASM^Op~BranchTestNotObject($valueReg, $failure));
  // emit MASM::UnboxObject(valueReg, objectReg);
  call $MASM^emit(1, $MASM^Op~UnboxObject($valueReg, $objectReg));
}

// op GuardShape(objectId: ObjectId, shapeField: ShapeField, label failure) {
procedure $CacheIR~GuardShape(
  $objectId: $ObjectId,
  $shapeField: $ShapeField,
  $failure: $MASM^Label
)
  modifies
    $MASM^pc,
    $MASM^ops,
    $MASM^pcEmitIds;
{
  var $objectReg: $Reg;
  var $shape: $Shape;

  // let objectReg = CacheIR::useObjectReg(objectId);
  $objectReg := $CacheIR~useObjectReg($objectId);
  // let shape = CacheIR::readShapeField(shapeField);
  $shape := $CacheIR~readShapeField($shapeField);

  // emit MASM::BranchTestNotObjectShape(objectReg, shape, failure);
  call $MASM^emit(2, $MASM^Op~BranchTestNotObjectShape($objectReg, $shape, $failure));
}

// op LoadFixedSlotResult(objectId: ObjectId, slotField: Int32Field) {
procedure $CacheIR~LoadFixedSlotResult($objectId: $ObjectId, $slotField: $Int32Field)
  modifies
    $MASM^pc,
    $MASM^ops,
    $MASM^pcEmitIds;
{
  var $objectReg: $Reg;
  var $slot: $Int32;

  // let objectReg = CacheIR::useObjectReg(objectId);
  $objectReg := $CacheIR~useObjectReg($objectId);
  // let slot = CacheIR::readInt32Field(slotField);
  $slot := $CacheIR~readInt32Field($slotField);

  // emit MASM::LoadObjectFixedSlot(objectReg, slot, CacheIR::outputReg);
  call $MASM^emit(3, $MASM^Op~LoadObjectFixedSlot($objectReg, $slot, $CacheIR~outputReg));
}

procedure {:entrypoint} $GetProp()
  modifies
    $MASM^pc,
    $MASM^ops,
    $MASM^pcEmitIds,
    $MASM^nextLabel,
    $MASM^labelPcs,
    $MASM~regs,
    $CacheIR~regAlloc;
{
  var $valueId: $ValueId;
  var $objectId: $ObjectId;

  var $valueReg: $ValueReg;
  var $objectReg: $Reg;

  /* ... test code ...
  var origValue: $Value;
  var origValueIsObject: $Bool;
  var origObject: $Object;
  var origObjectShape: $Shape;
  var origSuccess: $Bool;
  var origOutput: $Value;
  var finalOutput: $Value;
  */

  var $shapeField: $ShapeField;
  var $slotField: $Int32Field;

  var $shape: $Shape;
  var $class: $Class;
  var $slot: $Int32;

  var $failure: $MASM^Label;

  var op: $MASM^Op;

  call $valueReg := $CacheIR~allocateValueReg();
  call $objectReg := $CacheIR~allocateReg();

  assume $CacheIR~useValueReg($valueId) == $valueReg;
  assume $CacheIR~useObjectReg($objectId) == $objectReg;

  assume $CacheIR~readShapeField($shapeField) == $shape;
  assume $CacheIR~readInt32Field($slotField) == $slot;

  assume $Shape~hasFixedSlot($shape, $slot);
  assume $Shape~classOf($shape) == $class;
  assume $Class~isNativeObject($class);

  /* ... test code ...
  call origValue := $MASM~getValue($valueReg);
  call origValueIsObject := $Value~isObject(origValue);
  if (origValueIsObject) {
    call origObject := $Value~toObject(origValue);
    call origObjectShape := $Object~shapeOf(origObject);
    if (origObjectShape == $shape) {
      origSuccess := true;
      call origOutput := $Object~getFixedSlot(origObject, $slot);
    } else {
      origSuccess := false;
    }
  } else {
    origSuccess := false;
  }
  */

  assume (forall pc: $MASM^Pc :: $MASM^pcEmitIds[pc] == -1);

  $MASM^pc := 0;

  call $failure := $MASM^label();

  call $CacheIR~GuardToObject($valueId, $objectId, $failure);
  call $CacheIR~GuardShape($objectId, $shapeField, $failure);
  call $CacheIR~LoadFixedSlotResult($objectId, $slotField);

  call $MASM^emit(4, $MASM^Op^External());

  call $MASM^bind($failure);
  call $MASM^emit(5, $MASM^Op^External());

  $MASM^pc := 0;

  emit'0$MASM~BranchTestNotObject:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 0;
    op := $MASM^ops[$MASM^pc];
    call $MASM~BranchTestNotObject(
      $valueReg#$MASM^Op~BranchTestNotObject(op),
      $branch#$MASM^Op~BranchTestNotObject(op)
    );
    goto emit'1$MASM~UnboxObject, emit'5$MASM^External;

  emit'1$MASM~UnboxObject:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 1;
    op := $MASM^ops[$MASM^pc];
    call $MASM~UnboxObject(
      $valueReg#$MASM^Op~UnboxObject(op),
      $objectReg#$MASM^Op~UnboxObject(op)
    );

  emit'2$MASM~BranchTestNotObjectShape:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 2;
    op := $MASM^ops[$MASM^pc];
    call $MASM~BranchTestNotObjectShape(
      $objectReg#$MASM^Op~BranchTestNotObjectShape(op),
      $shape#$MASM^Op~BranchTestNotObjectShape(op),
      $branch#$MASM^Op~BranchTestNotObjectShape(op)
    );
    goto emit'3$MASM~LoadObjectFixedSlot, emit'5$MASM^External;

  emit'3$MASM~LoadObjectFixedSlot:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 3;
    op := $MASM^ops[$MASM^pc];
    call $MASM~LoadObjectFixedSlot(
      $objectReg#$MASM^Op~LoadObjectFixedSlot(op),
      $slot#$MASM^Op~LoadObjectFixedSlot(op),
      $outputReg#$MASM^Op~LoadObjectFixedSlot(op)
    );

  emit'4$MASM^External:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 4;
    /* ... test code ...
    assert origSuccess;
    call finalOutput := $MASM~getValue($CacheIR~outputReg);
    assert origOutput == finalOutput;
    */
    return;

  emit'5$MASM^External:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 5;
    /* ... test code ...
    assert !origSuccess;
    */
}
