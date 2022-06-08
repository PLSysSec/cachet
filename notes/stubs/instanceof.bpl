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

// fn isDouble(value: Value) -> Bool {
procedure $Value~isDouble($value: $Value)
  returns (ret: $Bool)
{
  var tmp'0: $ValueType;
  // [0] <- Value::typeOf(value)
  tmp'0 := $Value~typeOf($value);
  // [0] == ValueType::Double
  ret := tmp'0 == $ValueType~Double();
// }
}

// fn fromDouble(double: Double) -> Value {
procedure $Value~fromDouble($double: $Double)
  returns (ret: $Value)
{
  var $value: $Value;
  var tmp'0: $Bool;
  var tmp'1: $Double;
  // let value = unsafe { Value::fromDoubleUnchecked(object) };
  $value := $Value~fromDoubleUnchecked($double);
  // [0] <- Value::isDouble(value)
  call tmp'0 := $Value~isDouble($value);
  // assume [0];
  assume tmp'0;
  // [1] <- unsafe { Value::toDoubleUnchecked(value) }
  tmp'1 := $Value~toDoubleUnchecked($value);
  // assume [1] == double;
  assume tmp'1 == $double;
  // value
  ret := $value;
// }
}

// unsafe fn fromDoubleUnchecked(value: Double) -> Value;
function $Value~fromDoubleUnchecked($double: $Double): $Value;

// fn toDouble(value: Value) -> Double {
procedure $Value~toDouble($value: $Value)
  returns (ret: $Double)
{
  var $double: $Double;
  var tmp'0: $Bool;
  var tmp'1: $Value;
  // [0] <- Value::isDouble(value)
  call tmp'0 := $Value~isDouble($value);
  // assert [0];
  assert tmp'0;
  // let double = unsafe { Value::toDoubleUnchecked(value) };
  $double := $Value~toDoubleUnchecked($value);
  // [1] <- unsafe { Value::fromDoubleUnchecked(double) }
  tmp'1 := $Value~fromDoubleUnchecked($double);
  // assume [1] == value;
  assume tmp'1 == $value;
  // double
  ret := $double;
// }
}

// unsafe fn toDoubleUnchecked(value: Value) -> Double;
function $Value~toDoubleUnchecked($value: $Value): $Double;

// fn isInt32(value: Value) -> Bool {
procedure $Value~isInt32($value: $Value)
  returns (ret: $Bool)
{
  var tmp'0: $ValueType;
  // [0] <- Value::typeOf(value)
  tmp'0 := $Value~typeOf($value);
  // [0] == ValueType::Int32
  ret := tmp'0 == $ValueType~Int32();
// }
}

// fn fromInt32(int32: Int32) -> Value {
procedure $Value~fromInt32($int32: $Int32)
  returns (ret: $Value)
{
  var $value: $Value;
  var tmp'0: $Bool;
  var tmp'1: $Int32;
  // let value = unsafe { Value::fromInt32Unchecked(object) };
  $value := $Value~fromInt32Unchecked($int32);
  // [0] <- Value::isInt32(value)
  call tmp'0 := $Value~isInt32($value);
  // assume [0];
  assume tmp'0;
  // [1] <- unsafe { Value::toInt32Unchecked(value) }
  tmp'1 := $Value~toInt32Unchecked($value);
  // assume [1] == int32;
  assume tmp'1 == $int32;
  // value
  ret := $value;
// }
}

// unsafe fn fromInt32Unchecked(value: Int32) -> Value;
function $Value~fromInt32Unchecked($int32: $Int32): $Value;

// fn toInt32(value: Value) -> Int32 {
procedure $Value~toInt32($value: $Value)
  returns (ret: $Int32)
{
  var $int32: $Int32;
  var tmp'0: $Bool;
  var tmp'1: $Value;
  // [0] <- Value::isInt32(value)
  call tmp'0 := $Value~isInt32($value);
  // assert [0];
  assert tmp'0;
  // let int32 = unsafe { Value::toInt32Unchecked(value) };
  $int32 := $Value~toInt32Unchecked($value);
  // [1] <- unsafe { Value::fromInt32Unchecked(int32) }
  tmp'1 := $Value~fromInt32Unchecked($int32);
  // assume [1] == value;
  assume tmp'1 == $value;
  // int32
  ret := $int32;
// }
}

// unsafe fn toInt32Unchecked(value: Value) -> Int32;
function $Value~toInt32Unchecked($value: $Value): $Int32;

// fn isBool(value: Value) -> Bool {
procedure $Value~isBool($value: $Value)
  returns (ret: $Bool)
{
  var tmp'0: $ValueType;
  // [0] <- Value::typeOf(value)
  tmp'0 := $Value~typeOf($value);
  // [0] == ValueType::Bool
  ret := tmp'0 == $ValueType~Bool();
// }
}

// fn fromBool(bool: Bool) -> Value {
procedure $Value~fromBool($bool: $Bool)
  returns (ret: $Value)
{
  var $value: $Value;
  var tmp'0: $Bool;
  var tmp'1: $Bool;
  // let value = unsafe { Value::fromBoolUnchecked(object) };
  $value := $Value~fromBoolUnchecked($bool);
  // [0] <- Value::isBool(value)
  call tmp'0 := $Value~isBool($value);
  // assume [0];
  assume tmp'0;
  // [1] <- unsafe { Value::toBoolUnchecked(value) }
  tmp'1 := $Value~toBoolUnchecked($value);
  // assume [1] == bool;
  assume tmp'1 == $bool;
  // value
  ret := $value;
// }
}

// unsafe fn fromBoolUnchecked(value: Bool) -> Value;
function $Value~fromBoolUnchecked($bool: $Bool): $Value;

// fn toBool(value: Value) -> Bool {
procedure $Value~toBool($value: $Value)
  returns (ret: $Bool)
{
  var $bool: $Bool;
  var tmp'0: $Bool;
  var tmp'1: $Value;
  // [0] <- Value::isBool(value)
  call tmp'0 := $Value~isBool($value);
  // assert [0];
  assert tmp'0;
  // let bool = unsafe { Value::toBoolUnchecked(value) };
  $bool := $Value~toBoolUnchecked($value);
  // [1] <- unsafe { Value::fromBoolUnchecked(bool) }
  tmp'1 := $Value~fromBoolUnchecked($bool);
  // assume [1] == value;
  assume tmp'1 == $value;
  // bool
  ret := $bool;
// }
}

// unsafe fn toBoolUnchecked(value: Value) -> Bool;
function $Value~toBoolUnchecked($value: $Value): $Bool;

// fn isNull(value: Value) -> Bool {
procedure $Value~isNull($value: $Value)
  returns (ret: $Bool)
{
  var tmp'0: $ValueType;
  // [0] <- Value::typeOf(value)
  tmp'0 := $Value~typeOf($value);
  // [0] == ValueType::Null
  ret := tmp'0 == $ValueType~Null();
// }
}

// fn isMagic(value: Value) -> Bool {
procedure $Value~isMagic($value: $Value)
  returns (ret: $Bool)
{
  var tmp'0: $ValueType;
  // [0] <- Value::typeOf(value)
  tmp'0 := $Value~typeOf($value);
  // [0] == ValueType::Magic
  ret := tmp'0 == $ValueType~Magic();
// }
}

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

// fn getProto(object: Object) -> Value;
function $Object~getProto($object: $Object): $Value;

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

// fn getBool(reg: Reg) -> Bool {
procedure $MASM~getBool($reg: $Reg)
  returns (ret: $Bool)
{
  var tmp'0: $Value;
  // [0] <- MASM::getValue(reg)
  call tmp'0 := $MASM~getValue($reg);
  // Value::toBool([0])
  call ret := $Value~toBool(tmp'0);
// }
}

// fn setBool(reg: Reg, bool: Bool) {
procedure $MASM~setBool($reg: $Reg, $bool: $Bool)
  modifies $MASM~regs;
{
  var tmp'0: $Value;
  // [0] <- Value::fromBool(bool)
  call tmp'0 := $Value~fromBool($bool);
  // MASM::setValue(reg, [0]);
  call $MASM~setValue($reg, tmp'0);
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

// op Jump(label branch) {
function {:constructor} $MASM^Op~Jump($branch: $MASM^Label): $MASM^Op;

procedure $MASM~Jump($branch: $MASM^Label)
  modifies $MASM^pc;
{
  // goto branch;
  call $MASM^goto($branch);
  return;
  call $MASM^step();
// }
}

// op StoreBool(bool: Bool, dstReg: Reg) {
function {:constructor} $MASM^Op~StoreBool($bool: $Bool, $dstReg: $Reg): $MASM^Op;

procedure $MASM~StoreBool($bool: $Bool, $dstReg: $Reg)
  modifies $MASM^pc, $MASM~regs;
{
  // MASM::setBool(dstReg, bool);
  call $MASM~setBool($dstReg, $bool);
  call $MASM^step();
// }
}

// op StoreBoolValue(bool: Bool, dstReg: ValueReg) {
function {:constructor} $MASM^Op~StoreBoolValue($bool: $Bool, $dstReg: $ValueReg): $MASM^Op;

procedure $MASM~StoreBoolValue($bool: $Bool, $dstReg: $ValueReg)
  modifies $MASM^pc, $MASM~regs;
{
  var tmp'0: $Value;
  // [0] <- Value::fromBool(bool)
  call tmp'0 := $Value~fromBool($bool);
  // MASM::setValue(dstReg, [0]);
  call $MASM~setValue($dstReg, tmp'0);
  call $MASM^step();
// }
}

// op BranchTestNull(valueReg: ValueReg, label branch) {
function {:constructor} $MASM^Op~BranchTestNull($valueReg: $ValueReg, $branch: $MASM^Label):
  $MASM^Op;

procedure $MASM~BranchTestNull($valueReg: $ValueReg, $branch: $MASM^Label)
  modifies $MASM^pc;
{
  var $value: $Value;
  var tmp'0: $Bool;
  // let value = MASM::getValue(valueReg);
  call $value := $MASM~getValue($valueReg);
  // [0] <- Value::isNull(value)
  call tmp'0 := $Value~isNull($value);
  // if ([0]) {
  if (tmp'0) {
    // goto branch;
    call $MASM^goto($branch);
    return;
  // }
  }
  call $MASM^step();
// }
}

// op BranchTestMagic(valueReg: ValueReg, label branch) {
function {:constructor} $MASM^Op~BranchTestMagic($valueReg: $ValueReg, $branch: $MASM^Label): 
  $MASM^Op;

procedure $MASM~BranchTestMagic($valueReg: $ValueReg, $branch: $MASM^Label)
  modifies $MASM^pc;
{
  var $value: $Value;
  var tmp'0: $Bool;
  // let value = MASM::getValue(valueReg);
  call $value := $MASM~getValue($valueReg);
  // [0] <- Value::isMagic(value)
  call tmp'0 := $Value~isMagic($value);
  // if ([0]) {
  if (tmp'0) {
    // goto branch;
    call $MASM^goto($branch);
    return;
  // }
  }
  call $MASM^step();
// }
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

// op BranchTestObjectEq(lhsReg: Reg, rhsReg: Reg, label branch) {
function {:constructor} $MASM^Op~BranchTestObjectEq($lhsReg: $Reg, $rhsReg: $Reg, $branch: $MASM^Label): $MASM^Op;

procedure $MASM~BranchTestObjectEq(
  $lhsReg: $Reg,
  $rhsReg: $Reg,
  $branch: $MASM^Label
)
  modifies $MASM^pc;
{
  var $lhs: $Object;
  var $rhs: $Object;
  // let lhs = MASM::getObject(lhsReg);
  call $lhs := $MASM~getObject($lhsReg);
  // let rhs = MASM::getObject(rhsReg);
  call $rhs := $MASM~getObject($rhsReg);
  // if lhs == rhs {
  if ($lhs == $rhs) {
    // goto branch;
    call $MASM^goto($branch);
    return;
  // }
  }
  call $MASM^step();
// }
}

// op LoadObjectProto(objectReg: Reg, protoReg: ValueReg) {
function {:constructor} $MASM^Op~LoadObjectProto($objectReg: $Reg, $protoReg: $ValueReg): $MASM^Op;

procedure $MASM~LoadObjectProto($objectReg: $Reg, $protoReg: $ValueReg)
  modifies $MASM^pc, $MASM~regs;
{
  var $object: $Object;
  var tmp'0: $Value;
  // let object = MASM::getObject(objectReg);
  call $object := $MASM~getObject($objectReg);
  // [0] <- Object::getProto(object)
  tmp'0 := $Object~getProto($object);
  // MASM::setValue(protoReg, [0]);
  call $MASM~setValue($protoReg, tmp'0);
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

// ir CacheIR emits MASM {

// fn allocateValueReg() -> ValueReg {
procedure $CacheIR~allocateValueReg()
  returns (ret: $ValueReg)
  modifies $CacheIR~allocatedRegs;
{
  // CacheIR::allocateReg()
  call ret := $CacheIR~allocateReg();
// }
}

// fn releaseValueReg(valueReg: ValueReg) {
procedure $CacheIR~releaseValueReg($valueReg: $ValueReg)
  modifies $CacheIR~allocatedRegs;
{
  // CacheIR::releaseReg(valueReg);
  call $CacheIR~releaseReg($valueReg);
// }
}

// var mut allocatedRegs: Set<Reg>;
var $CacheIR~allocatedRegs: $Set $Reg;

// fn allocateReg() -> Reg {
procedure $CacheIR~allocateReg()
  returns (ret: $Reg)
  modifies $CacheIR~allocatedRegs;
{
  var $reg: $Reg;
  var tmp'0: $Bool;
  // let reg = unsafe { CacheIR::allocateRegUnchecked(CacheIR::allocatedRegs) };
  $reg := $CacheIR~allocateRegUnchecked($CacheIR~allocatedRegs);
  // [0] <- Set::contains(CacheIR::allocatedRegs, reg)
  tmp'0 := $Set~contains($CacheIR~allocatedRegs, $reg);
  // assume ![0];
  assume !tmp'0;
  // CacheIR::allocatedRegs = Set::add(CacheIR::allocatedRegs, reg);
  $CacheIR~allocatedRegs := $Set~add($CacheIR~allocatedRegs, $reg);
  // reg
  ret := $reg;
// }
}

// unsafe fn allocateRegUnchecked(allocatedRegs: Set<Reg>) -> Reg;
function $CacheIR~allocateRegUnchecked($allocatedRegs: $Set $Reg): $Reg;

// fn releaseReg(reg: Reg) {
procedure $CacheIR~releaseReg($reg: $Reg)
  modifies $CacheIR~allocatedRegs;
{
  // CacheIR::allocatedRegs = Set::remove(CacheIR::allocatedRegs, reg);
  $CacheIR~allocatedRegs := $Set~remove($CacheIR~allocatedRegs, $reg);
// }
}

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

// op LoadInstanceOfObjectResult(lhsId: ValueId, protoId: ObjectId, label failure) {
procedure $CacheIR~LoadInstanceOfObjectResult(
  $lhsId: $ValueId,
  $protoId: $ObjectId,
  $failure: $MASM^Label
)
  modifies
    $MASM^pc,
    $MASM^ops,
    $MASM^pcEmitIds,
    $MASM^nextLabel,
    $MASM^labelPcs,
    $MASM~regs,
    $CacheIR~allocatedRegs;
{
  var $lhsReg: $ValueReg;
  var $protoReg: $Reg;
  var $scratchReg: $Reg;
  var $loop: $MASM^Label;
  var $returnFalse: $MASM^Label;
  var $returnTrue: $MASM^Label;
  var $done: $MASM^Label;

  // let lhsReg = CacheIR::useValueReg(lhsId);
  $lhsReg := $CacheIR~useValueReg($lhsId);
  // let protoReg = CacheIR::useObjectReg(protoId);
  $protoReg := $CacheIR~useObjectReg($protoId);
  // let scratchReg = CacheIR::allocateReg();
  call $scratchReg := $CacheIR~allocateReg();

  // label loop;
  call $loop := $MASM^label();
  // label returnFalse;
  call $returnFalse := $MASM^label();
  // label returnTrue;
  call $returnTrue := $MASM^label();
  // label done;
  call $done := $MASM^label();

  // emit MASM::BranchTestNotObject(lhsReg, returnFalse);
  call $MASM^emit(0, $MASM^Op~BranchTestNotObject($lhsReg, $returnFalse));
  // emit MASM::UnboxObject(lhsReg, scratchReg);
  call $MASM^emit(1, $MASM^Op~UnboxObject($lhsReg, $scratchReg));
  // emit MASM::LoadObjectProto(scratchReg, lhsReg);
  call $MASM^emit(2, $MASM^Op~LoadObjectProto($scratchReg, $lhsReg));

  // bind loop;
  call $MASM^bind($loop);
  // emit MASM::BranchTestNull(lhsReg, returnFalse);
  call $MASM^emit(3, $MASM^Op~BranchTestNull($lhsReg, $returnFalse));
  // emit MASM::BranchTestMagic(lhsReg, failure);
  call $MASM^emit(4, $MASM^Op~BranchTestMagic($lhsReg, $failure));

  // emit MASM::BranchTestNotObject(lhsReg, returnFalse);
  call $MASM^emit(5, $MASM^Op~BranchTestNotObject($lhsReg, $returnFalse));
  // emit MASM::UnboxObject(lhsReg, scratchReg);
  call $MASM^emit(6, $MASM^Op~UnboxObject($lhsReg, $scratchReg));
  // emit MASM::BranchTestObjectEq(scratchReg, protoReg, returnTrue);
  call $MASM^emit(7, $MASM^Op~BranchTestObjectEq($scratchReg, $protoReg, $returnTrue));

  // emit MASM::LoadObjectProto(scratchReg, lhsReg);
  call $MASM^emit(8, $MASM^Op~LoadObjectProto($scratchReg, $lhsReg));
  // emit MASM::Jump(loop);
  call $MASM^emit(9, $MASM^Op~Jump($loop));

  // bind returnFalse;
  call $MASM^bind($returnFalse);
  // emit MASM::StoreBool(false, CacheIR::outputReg);
  call $MASM^emit(10, $MASM^Op~StoreBool(false, $CacheIR~outputReg));
  // emit MASM::Jump(done);
  call $MASM^emit(11, $MASM^Op~Jump($done));

  // bind returnTrue;
  call $MASM^bind($returnTrue);
  // emit MASM::StoreBool(true, CacheIR::outputReg);
  call $MASM^emit(12, $MASM^Op~StoreBool(true, $CacheIR~outputReg));

  // bind done;
  call $MASM^bind($done);

  // CacheIR::releaseReg(scratchReg);
  call $CacheIR~releaseReg($scratchReg);
}

procedure {:entrypoint} $InstanceOf($lhsId: $ValueId, $protoId: $ObjectId)
  modifies
    $MASM^pc,
    $MASM^ops,
    $MASM^pcEmitIds,
    $MASM^nextLabel,
    $MASM^labelPcs,
    $MASM~regs,
    $CacheIR~allocatedRegs;
{
  var $lhsReg: $ValueReg;
  var $protoReg: $Reg;

  var $proto: $Object;

  var $failure: $MASM^Label;

  var op: $MASM^Op;


  /* ... test code ...
  var origLhs: $Value;
  var origLhsIsObject: $Bool;
  var origLhsObject: $Object;
  var origLhsProto: $Value;
  var origLhsProtoIsObject: $Bool;
  var origLhsProtoObject: $Object;
  var origOutput: $Bool;
  var finalOutput: $Bool;
  */

  call $lhsReg := $CacheIR~allocateValueReg();
  call $protoReg := $CacheIR~allocateReg();

  assume $CacheIR~useValueReg($lhsId) == $lhsReg;
  assume $CacheIR~useObjectReg($protoId) == $protoReg;

  assume $Value~typeOf($MASM~regs[$protoReg]) == $ValueType~Object();

  call $proto := $MASM~getObject($protoReg);

  /* ... test code ...
  origOutput := false;
  call origLhs := $MASM~getValue($CacheIR~useValueReg($lhsId));
  call origLhsIsObject := $Value~isObject(origLhs);
  if (origLhsIsObject) {
    call origLhsObject := $Value~toObject(origLhs);
    origLhsProto := $Object~getProto(origLhsObject);
    call origLhsProtoIsObject := $Value~isObject(origLhsProto);
    if (origLhsProtoIsObject) {
      call origLhsProtoObject := $Value~toObject(origLhsProto);
      origOutput := origLhsProtoObject == $proto;
    }
  }
  */

  assume (forall pc: $MASM^Pc :: $MASM^pcEmitIds[pc] == -1);

  $MASM^pc := 0;

  call $failure := $MASM^label();

  call $CacheIR~LoadInstanceOfObjectResult($lhsId, $protoId, $failure);

  call $MASM^emit(13, $MASM^Op^External());

  call $MASM^bind($failure);
  call $MASM^emit(14, $MASM^Op^External());

  $MASM^pc := 0;

  emit'0$MASM~BranchTestNotObject:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 0;
    op := $MASM^ops[$MASM^pc];
    call $MASM~BranchTestNotObject(
      $valueReg#$MASM^Op~BranchTestNotObject(op),
      $branch#$MASM^Op~BranchTestNotObject(op)
    );
    goto emit'1$MASM~UnboxObject, emit'10$MASM~StoreBool;

  emit'1$MASM~UnboxObject:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 1;
    op := $MASM^ops[$MASM^pc];
    call $MASM~UnboxObject(
      $valueReg#$MASM^Op~UnboxObject(op),
      $objectReg#$MASM^Op~UnboxObject(op)
    );

  emit'2$MASM~LoadObjectProto:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 2;
    op := $MASM^ops[$MASM^pc];
    call $MASM~LoadObjectProto(
      $objectReg#$MASM^Op~LoadObjectProto(op),
      $protoReg#$MASM^Op~LoadObjectProto(op)
    );

  assert $MASM~regs[$protoReg] == $Value~fromObjectUnchecked($proto);

  emit'3$MASM~BranchTestNull:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 3;
    assume $MASM~regs[$protoReg] == $Value~fromObjectUnchecked($proto);
    op := $MASM^ops[$MASM^pc];
    call $MASM~BranchTestNull(
      $valueReg#$MASM^Op~BranchTestNull(op),
      $branch#$MASM^Op~BranchTestNull(op)
    );
    goto emit'4$MASM~BranchTestMagic, emit'10$MASM~StoreBool;

  emit'4$MASM~BranchTestMagic:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 4;
    op := $MASM^ops[$MASM^pc];
    call $MASM~BranchTestMagic(
      $valueReg#$MASM^Op~BranchTestMagic(op),
      $branch#$MASM^Op~BranchTestMagic(op)
    );
    goto emit'5$MASM~BranchTestNotObject, emit'14$MASM^External;

  emit'5$MASM~BranchTestNotObject:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 5;
    op := $MASM^ops[$MASM^pc];
    call $MASM~BranchTestNotObject(
      $valueReg#$MASM^Op~BranchTestNotObject(op),
      $branch#$MASM^Op~BranchTestNotObject(op)
    );
    goto emit'6$MASM~UnboxObject, emit'10$MASM~StoreBool;

  emit'6$MASM~UnboxObject:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 6;
    op := $MASM^ops[$MASM^pc];
    call $MASM~UnboxObject(
      $valueReg#$MASM^Op~UnboxObject(op),
      $objectReg#$MASM^Op~UnboxObject(op)
    );

  emit'7$MASM~BranchTestObjectEq:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 7;
    op := $MASM^ops[$MASM^pc];
    call $MASM~BranchTestObjectEq(
      $lhsReg#$MASM^Op~BranchTestObjectEq(op),
      $rhsReg#$MASM^Op~BranchTestObjectEq(op),
      $branch#$MASM^Op~BranchTestObjectEq(op)
    );
    goto emit'8$MASM~LoadObjectProto, emit'12$MASM~StoreBool;

  emit'8$MASM~LoadObjectProto:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 8;
    op := $MASM^ops[$MASM^pc];
    call $MASM~LoadObjectProto(
      $objectReg#$MASM^Op~LoadObjectProto(op),
      $protoReg#$MASM^Op~LoadObjectProto(op)
    );

  emit'9$MASM~Jump:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 9;
    op := $MASM^ops[$MASM^pc];
    call $MASM~Jump($branch#$MASM^Op~Jump(op));
    assert $MASM~regs[$protoReg] == $Value~fromObjectUnchecked($proto);
    goto emit'3$MASM~BranchTestNull;

  emit'10$MASM~StoreBool:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 10;
    op := $MASM^ops[$MASM^pc];
    call $MASM~StoreBool(
      $bool#$MASM^Op~StoreBool(op),
      $dstReg#$MASM^Op~StoreBool(op)
    );

  emit'11$MASM~Jump:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 11;
    op := $MASM^ops[$MASM^pc];
    call $MASM~Jump($branch#$MASM^Op~Jump(op));
    goto emit'13$MASM^External;

  emit'12$MASM~StoreBool:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 12;
    op := $MASM^ops[$MASM^pc];
    call $MASM~StoreBool(
      $bool#$MASM^Op~StoreBool(op),
      $dstReg#$MASM^Op~StoreBool(op)
    );

  emit'13$MASM^External:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 13;
    /* ... test code ...
    if (origOutput) {
      call finalOutput := $MASM~getBool($CacheIR~outputReg);
      assert finalOutput;
    }
    */
    return;

  emit'14$MASM^External:
    assume {:partition} $MASM^pcEmitIds[$MASM^pc] == 14;
    /* ... test code ...
    assert !origOutput;
    */
}
