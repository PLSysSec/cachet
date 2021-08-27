type Label;

type {:datatype} MaybeLabel;
function {:constructor} NoLabel(): MaybeLabel;
function {:constructor} SomeLabel(label: Label): MaybeLabel;

var jump: MaybeLabel;

type Heap;
var heap: Heap;
procedure ObserveHeapWrite();
  modifies heap;

type $Bool = bool;
type $Int32 = bv32;
type $Double = float53e11; // 64-bit; see https://github.com/boogie-org/boogie/issues/29#issuecomment-231239065

function {:bvbuiltin "bvadd"} $Int32^Add(x: $Int32, y: $Int32): $Int32;
// etc; see https://boogie-docs.readthedocs.io/en/latest/LangRef.html#other-operators

type {:datatype} $ValueType;
function {:constructor} $ValueType~Double(): $ValueType;
function {:constructor} $ValueType~Int32(): $ValueType;
function {:constructor} $ValueType~Boolean(): $ValueType;
function {:constructor} $ValueType~Undefined(): $ValueType;
function {:constructor} $ValueType~Null(): $ValueType;
function {:constructor} $ValueType~Magic(): $ValueType;
function {:constructor} $ValueType~String(): $ValueType;
function {:constructor} $ValueType~Symbol(): $ValueType;
function {:constructor} $ValueType~PrivateGCThing(): $ValueType;
function {:constructor} $ValueType~BigInt(): $ValueType;
function {:constructor} $ValueType~Object(): $ValueType;

type $Value;

function $Value~typeOf($value: $Value): $ValueType;

procedure $Value~isObject($value: $Value)
  returns (ret: $Bool)
{
  ret := $Value~typeOf($value) == $ValueType~Object();
  return;
}

procedure $Value~fromObject($object: $Object)
  returns (ret: $Value)
{
  var $value: $Value;
  var tmp'0: $Bool;
  $value := $Value~fromObjectUnchecked($object);
  call tmp'0 := $Value~isObject($value);
  assume tmp'0;
  assume $Value~toObjectUnchecked($value) == $object;
  ret := $value;
  return;
}

function $Value~fromObjectUnchecked($object: $Object): $Value;

procedure $Value~toObject($value: $Value)
  returns (ret: $Object)
{
  var $object: $Object;
  var tmp'0: $Bool;
  call tmp'0 := $Value~isObject($value);
  assert tmp'0;
  $object := $Value~toObjectUnchecked($value);
  assume $Value~fromObjectUnchecked($object) == $value;
  ret := $object;
  return;
}

function $Value~toObjectUnchecked($value: $Value): $Object;

type $Object;

function $Object~shapeOf(heap: Heap, $object: $Object): $Shape;

procedure $Object~toNativeObject($object: $Object)
  returns (ret: $NativeObject)
{
  var $shape: $Shape;
  var $class: $Class;
  $shape := $Object~shapeOf(heap, $object);
  $class := $Shape~classOf($shape);
  assert $Class~isNativeObject($class);
  ret := $NativeObject^from$Object($object);
  return;
}

procedure $Object~getFixedSlot($object: $Object, $slot: $Int32)
  returns (ret: $Value)
{
  var $nativeObject: $NativeObject;
  var tmp'0: $NativeObject;
  var tmp'1: $Value;
  call tmp'0 := $Object~toNativeObject($object);
  $nativeObject := tmp'0;
  call tmp'1 := $NativeObject~getFixedSlot($nativeObject, $slot);
  ret := tmp'1;
  return;
}

type $NativeObject;

function $NativeObject^from$Object(in: $Object): $NativeObject;
function $NativeObject^to$Object(in: $NativeObject): $Object;
axiom (forall in: $NativeObject :: $NativeObject^from$Object($NativeObject^to$Object(in)) == in);
axiom (forall in: $Object :: $NativeObject^to$Object($NativeObject^from$Object(in)) == in);

procedure $NativeObject~getFixedSlot($nativeObject: $NativeObject, $slot: $Int32)
  returns (ret: $Value)
{
  var $shape: $Shape;
  $shape := $Object~shapeOf(heap, $NativeObject^to$Object($nativeObject));
  assert $Shape~hasFixedSlot($shape, $slot);
  ret := $NativeObject~getFixedSlotUnchecked(heap, $nativeObject, $slot);
  return;
}

function $NativeObject~getFixedSlotUnchecked(heap: Heap, $nativeObject: $NativeObject, $slot: $Int32): $Value;

type $Shape;

function $Shape~classOf($shape: $Shape): $Class;

function $Shape~hasFixedSlot($shape: $Shape, $slot: $Int32): $Bool;

type $Class;

function $Class~isNativeObject($class: $Class): $Bool;

type $ValueReg;

var valueRegs: [$ValueReg]$Value;

procedure $ValueReg~getValue($valueReg: $ValueReg)
  returns (ret: $Value)
{ 
  ret := valueRegs[$valueReg];
  return;
}

procedure $ValueReg~setValue($valueReg: $ValueReg, $value: $Value)
  modifies valueRegs;
{
  valueRegs[$valueReg] := $value;
  return;
}

type $Reg;

var regs: [$Reg]$Value;

procedure $Reg~getObject($reg: $Reg)
  returns (ret: $Object)
{ 
  var tmp'0: $Object;
  call tmp'0 := $Value~toObject(regs[$reg]);
  ret := tmp'0;
  return;
}

procedure $Reg~setObject($reg: $Reg, $object: $Object)
  modifies regs;
{
  var tmp'0: $Value;
  call tmp'0 := $Value~fromObject($object);
  regs[$reg] := tmp'0;
  return;
}

procedure $MASM~branchTestNotObject($valueReg: $ValueReg, $label: Label)
  modifies jump;
{
  var $value: $Value;
  var tmp'0: $Value;
  var tmp'1: $Bool;
  call tmp'0 := $ValueReg~getValue($valueReg);
  $value := tmp'0;
  call tmp'1 := $Value~isObject($value);
  if (!tmp'1) {
    jump := SomeLabel($label);
    return;
  }
  return;
}

procedure $MASM~unboxObject($valueReg: $ValueReg, $objectReg: $Reg)
  modifies regs;
{
  var $value: $Value;
  var tmp'0: $Value;
  var tmp'1: $Object;
  call tmp'0 := $ValueReg~getValue($valueReg);
  $value := tmp'0;
  call tmp'1 := $Value~toObject($value);
  call $Reg~setObject($objectReg, tmp'1);
  return;
}

procedure $MASM~branchTestNotObjectShape($objectReg: $Reg, $shape: $Shape, $label: Label)
  modifies jump;
{
  var $object: $Object;
  var tmp'0: $Object;
  call tmp'0 := $Reg~getObject($objectReg);
  $object := tmp'0;
  if ($Object~shapeOf(heap, $object) != $shape) {
    jump := SomeLabel($label);
    return;
  }
  return;
}

procedure $MASM~loadObjectFixedSlot($objectReg: $Reg, $slot: $Int32, $outputReg: $ValueReg)
  modifies regs;
{
  var $object: $Object;
  var tmp'0: $Object;
  var tmp'1: $Value;
  call tmp'0 := $Reg~getObject($objectReg);
  $object := tmp'0;
  call tmp'1 := $Object~getFixedSlot($object, $slot);
  call $ValueReg~setValue($outputReg, tmp'1);
  return;
}

type $ValId;

function $ValId~toReg($valId: $ValId): $ValueReg;

type $ObjId;

function $ObjId~toReg($objId: $ObjId): $Reg;

type $Int32Field;

function $Int32Field~toInt32($int32Field: $Int32Field): $Int32;

type $ShapeField;

function $ShapeField~toShape($shapeField: $ShapeField): $Shape;

procedure $CacheIR~guardToObject($valId: $ValId, $objId: $ObjId, $failure: Label)
  modifies regs;
  modifies jump;
{
  var $val: $ValueReg;
  var $obj: $Reg;
  $val := $ValId~toReg($valId);
  $obj := $ObjId~toReg($objId);
  call $MASM~branchTestNotObject($val, $failure);
  if (is#SomeLabel(jump)) {
    return;
  }
  call $MASM~unboxObject($val, $obj);
  return;
}

procedure $CacheIR~guardShape($objId: $ObjId, $shapeField: $ShapeField, $failure: Label)
  modifies jump;
{
  var $obj: $Reg;
  var $shape: $Shape;
  $obj := $ObjId~toReg($objId);
  $shape := $ShapeField~toShape($shapeField);
  call $MASM~branchTestNotObjectShape($obj, $shape, $failure);
  if (is#SomeLabel(jump)) {
    return;
  }
  return;
}

procedure $CacheIR~loadFixedSlotResult($objId: $ObjId, $slotField: $Int32Field, $output: $ValueReg)
  modifies valueRegs;
{
  var $obj: $Reg;
  var $slot: $Int32;
  $obj := $ObjId~toReg($objId);
  $slot := $Int32Field~toInt32($slotField);
  call $MASM~loadObjectFixedSlot($obj, $slot, $output);
  return;
}

const $shape: $Shape;
const $class: $Class;
const $slot: $Int32;

axiom $Shape~hasFixedSlot($shape, $slot);
axiom $Shape~classOf($shape) == $class;
axiom $Class~isNativeObject($class);

const $shapeField: $ShapeField;
const $slotField: $Int32Field;

axiom $ShapeField~toShape($shapeField) == $shape;
axiom $Int32Field~toInt32($slotField) == $slot;

procedure $GetProp($valId: $ValId, $objId: $ObjId, $output: $ValueReg, $failure: Label)
  modifies heap;
  modifies valueRegs;
  modifies regs;
  modifies jump;
{
  var first: $Value;
  var second: $Value;
  call $CacheIR~guardToObject($valId, $objId, $failure);
  assume !is#SomeLabel(jump);
  call $CacheIR~guardShape($objId, $shapeField, $failure);
  assume !is#SomeLabel(jump);
  call $CacheIR~loadFixedSlotResult($objId, $slotField, $output);
  first := valueRegs[$output];
  //call ObserveHeapWrite();
  call $CacheIR~loadFixedSlotResult($objId, $slotField, $output);
  second := valueRegs[$output];
  assert first == second;
  return;
}
