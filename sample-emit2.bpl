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

function $Int32~toDouble($int32: $Int32): $Double;

type $Value;

function $Value~typeOf($value: $Value): $ValueType;

procedure $Value~isInt32($value: $Value)
  returns (ret: $Bool)
{
  ret := $Value~typeOf($value) == $ValueType~Int32();
}

procedure $Value~fromInt32($int32: $Int32)
  returns (ret: $Value)
{
  var $value: $Value;
  var tmp'0: $Bool;
  $value := $Value~fromInt32Unchecked($int32);
  call tmp'0 := $Value~isInt32($value);
  assume tmp'0;
  assume $Value~toInt32Unchecked($value) == $int32;
  ret := $value;
}

function $Value~fromInt32Unchecked($int32: $Int32): $Value;

procedure $Value~toInt32($value: $Value)
  returns (ret: $Int32)
{
  var $int32: $Int32;
  var tmp'0: $Bool;
  call tmp'0 := $Value~isInt32($value);
  assert tmp'0;
  $int32 := $Value~toInt32Unchecked($value);
  assume $Value~fromInt32Unchecked($int32) == $value;
  ret := $int32;
}

function $Value~toInt32Unchecked($value: $Value): $Int32;

procedure $Value~isDouble($value: $Value)
  returns (ret: $Bool)
{
  ret := $Value~typeOf($value) == $ValueType~Double();
}

procedure $Value~fromDouble($double: $Double)
  returns (ret: $Value)
{
  var $value: $Value;
  var tmp'0: $Bool;
  $value := $Value~fromDoubleUnchecked($double);
  call tmp'0 := $Value~isDouble($value);
  assume tmp'0;
  assume $Value~toDoubleUnchecked($value) == $double;
  ret := $value;
}

function $Value~fromDoubleUnchecked($double: $Double): $Value;

procedure $Value~toDouble($value: $Value)
  returns (ret: $Double)
{
  var $double: $Double;
  var tmp'0: $Bool;
  call tmp'0 := $Value~isDouble($value);
  assert tmp'0;
  $double := $Value~toDoubleUnchecked($value);
  assume $Value~fromDoubleUnchecked($double) == $value;
  ret := $double;
}

function $Value~toDoubleUnchecked($value: $Value): $Double;

procedure $Value~isObject($value: $Value)
  returns (ret: $Bool)
{
  ret := $Value~typeOf($value) == $ValueType~Object();
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
}

procedure $ValueReg~setValue($valueReg: $ValueReg, $value: $Value)
  modifies valueRegs;
{
  valueRegs[$valueReg] := $value;
}

type $Reg;

var regs: [$Reg]$Value;

procedure $Reg~getInt32($reg: $Reg)
  returns (ret: $Int32)
{ 
  var tmp'0: $Int32;
  call tmp'0 := $Value~toInt32(regs[$reg]);
  ret := tmp'0;
}

procedure $Reg~setInt32($reg: $Reg, $int32: $Int32)
  modifies regs;
{
  var tmp'0: $Value;
  call tmp'0 := $Value~fromInt32($int32);
  regs[$reg] := tmp'0;
}

procedure $Reg~getDouble($reg: $Reg)
  returns (ret: $Double)
{ 
  var tmp'0: $Double;
  call tmp'0 := $Value~toDouble(regs[$reg]);
  ret := tmp'0;
}

procedure $Reg~setDouble($reg: $Reg, $double: $Double)
  modifies regs;
{
  var tmp'0: $Value;
  call tmp'0 := $Value~fromDouble($double);
  regs[$reg] := tmp'0;
}

procedure $Reg~getObject($reg: $Reg)
  returns (ret: $Object)
{ 
  var tmp'0: $Object;
  call tmp'0 := $Value~toObject(regs[$reg]);
  ret := tmp'0;
}

procedure $Reg~setObject($reg: $Reg, $object: $Object)
  modifies regs;
{
  var tmp'0: $Value;
  call tmp'0 := $Value~fromObject($object);
  regs[$reg] := tmp'0;
}

type {:datatype} $MASM^Op;

function {:constructor} $MASM^Op^noOp(): $MASM^Op;

type $MASM^Pc = int;

var $MASM^ops: [$MASM^Pc]$MASM^Op;
var $MASM^emitPc: $MASM^Pc;
var $MASM^pc: $MASM^Pc;

procedure $MASM^emit(op: $MASM^Op)
  modifies $MASM^ops;
  modifies $MASM^emitPc;
{
  $MASM^ops[$MASM^emitPc] := op;
  $MASM^emitPc := $MASM^emitPc + 1;
}

procedure {:inline 1} $MASM^step()
  modifies $MASM^pc;
{
  $MASM^pc := $MASM^pc + 1;
}

type $MASM^Label = int;

var $MASM^nextLabel: $MASM^Label;
var $MASM^labels: [$MASM^Label]$MASM^Pc;

procedure $MASM^label()
  returns (label: $MASM^Label)
  modifies $MASM^nextLabel;
{
  label := $MASM^nextLabel;
  $MASM^nextLabel := $MASM^nextLabel + 1;
}

procedure $MASM^bind(label: $MASM^Label)
  modifies $MASM^labels;
{
  $MASM^labels[label] := $MASM^emitPc;
}

procedure {:inline 1} $MASM^jump(label: $MASM^Label)
  modifies $MASM^pc;
{
  $MASM^pc := $MASM^labels[label];
}

function {:constructor} $MASM^Op~jump($label: $MASM^Label): $MASM^Op;

procedure $MASM~jump($label: $MASM^Label)
  modifies $MASM^pc;
{
  call $MASM^jump($label);
}

function {:constructor} $MASM^Op~branchTestNotInt32($valueReg: $ValueReg, $label: $MASM^Label): $MASM^Op;

procedure $MASM~branchTestNotInt32($valueReg: $ValueReg, $label: $MASM^Label)
  modifies $MASM^pc;
{
  var $value: $Value;
  var tmp'0: $Value;
  var tmp'1: $Bool;
  call tmp'0 := $ValueReg~getValue($valueReg);
  $value := tmp'0;
  call tmp'1 := $Value~isInt32($value);
  if (!tmp'1) {
    call $MASM^jump($label);
    return;
  }
  call $MASM^step();
}

function {:constructor} $MASM^Op~branchTestNotDouble($valueReg: $ValueReg, $label: $MASM^Label): $MASM^Op;

procedure $MASM~branchTestNotDouble($valueReg: $ValueReg, $label: $MASM^Label)
  modifies $MASM^pc;
{
  var $value: $Value;
  var tmp'0: $Value;
  var tmp'1: $Bool;
  call tmp'0 := $ValueReg~getValue($valueReg);
  $value := tmp'0;
  call tmp'1 := $Value~isDouble($value);
  if (!tmp'1) {
    call $MASM^jump($label);
    return;
  }
  call $MASM^step();
}

function {:constructor} $MASM^Op~branchTestNotObject($valueReg: $ValueReg, $label: $MASM^Label): $MASM^Op;

procedure $MASM~branchTestNotObject($valueReg: $ValueReg, $label: $MASM^Label)
  modifies $MASM^pc;
{
  var $value: $Value;
  var tmp'0: $Value;
  var tmp'1: $Bool;
  call tmp'0 := $ValueReg~getValue($valueReg);
  $value := tmp'0;
  call tmp'1 := $Value~isObject($value);
  if (!tmp'1) {
    call $MASM^jump($label);
    return;
  }
  call $MASM^step();
}

function {:constructor} $MASM^Op~branchTestNotObjectShape($objectReg: $Reg, $shape: $Shape, $label: $MASM^Label): $MASM^Op;

procedure $MASM~branchTestNotObjectShape($objectReg: $Reg, $shape: $Shape, $label: $MASM^Label)
  modifies $MASM^pc;
{
  var $object: $Object;
  var tmp'0: $Object;
  call tmp'0 := $Reg~getObject($objectReg);
  $object := tmp'0;
  if ($Object~shapeOf(heap, $object) != $shape) {
    call $MASM^jump($label);
    return;
  }
  call $MASM^step();
}

function {:constructor} $MASM^Op~unboxInt32($valueReg: $ValueReg, $int32Reg: $Reg): $MASM^Op;

procedure $MASM~unboxInt32($valueReg: $ValueReg, $int32Reg: $Reg)
  modifies regs;
  modifies $MASM^pc;
{
  var $value: $Value;
  var tmp'0: $Value;
  var tmp'1: $Int32;
  call tmp'0 := $ValueReg~getValue($valueReg);
  $value := tmp'0;
  call tmp'1 := $Value~toInt32($value);
  call $Reg~setInt32($int32Reg, tmp'1);
  call $MASM^step();
}

function {:constructor} $MASM^Op~unboxDouble($valueReg: $ValueReg, $doubleReg: $Reg): $MASM^Op;

procedure $MASM~unboxDouble($valueReg: $ValueReg, $doubleReg: $Reg)
  modifies regs;
  modifies $MASM^pc;
{
  var $value: $Value;
  var tmp'0: $Value;
  var tmp'1: $Double;
  call tmp'0 := $ValueReg~getValue($valueReg);
  $value := tmp'0;
  call tmp'1 := $Value~toDouble($value);
  call $Reg~setDouble($doubleReg, tmp'1);
  call $MASM^step();
}

function {:constructor} $MASM^Op~unboxObject($valueReg: $ValueReg, $objectReg: $Reg): $MASM^Op;

procedure $MASM~unboxObject($valueReg: $ValueReg, $objectReg: $Reg)
  modifies regs;
  modifies $MASM^pc;
{
  var $value: $Value;
  var tmp'0: $Value;
  var tmp'1: $Object;
  call tmp'0 := $ValueReg~getValue($valueReg);
  $value := tmp'0;
  call tmp'1 := $Value~toObject($value);
  call $Reg~setObject($objectReg, tmp'1);
  call $MASM^step();
}

function {:constructor} $MASM^Op~loadObjectFixedSlot($objectReg: $Reg, $slot: $Int32, $valueReg: $ValueReg): $MASM^Op;

procedure $MASM~loadObjectFixedSlot($objectReg: $Reg, $slot: $Int32, $valueReg: $ValueReg)
  modifies valueRegs;
  modifies $MASM^pc;
{
  var $object: $Object;
  var tmp'0: $Object;
  var tmp'1: $Value;
  call tmp'0 := $Reg~getObject($objectReg);
  $object := tmp'0;
  call tmp'1 := $Object~getFixedSlot($object, $slot);
  call $ValueReg~setValue($valueReg, tmp'1);
  call $MASM^step();
}

type $ValId;

function $ValId~toValueReg($valId: $ValId): $ValueReg;

type $ObjId;

function $ObjId~toReg($objId: $ObjId): $Reg;

type $Int32Field;

function $Int32Field~toInt32($int32Field: $Int32Field): $Int32;

type $ShapeField;

function $ShapeField~toShape($shapeField: $ShapeField): $Shape;

procedure $CacheIR~guardToObject($valId: $ValId, $objId: $ObjId, $failure: $MASM^Label)
  modifies $MASM^ops;
  modifies $MASM^emitPc;
{
  var $val: $ValueReg;
  var $obj: $Reg;
  var $knownVal: $Value;
  var $skipGuard: $Bool;
  $val := $ValId~toValueReg($valId);
  $obj := $ObjId~toReg($objId);
  call $knownVal := $ValueReg~getValue($val);
  call $skipGuard := $Value~isObject($knownVal);
  if (!$skipGuard) {
    call $MASM^emit($MASM^Op~branchTestNotObject($val, $failure));
  }
  call $MASM^emit($MASM^Op~unboxObject($val, $obj));
}

procedure $CacheIR~guardShape($objId: $ObjId, $shapeField: $ShapeField, $failure: $MASM^Label)
  modifies $MASM^ops;
  modifies $MASM^emitPc;
{
  var $obj: $Reg;
  var $shape: $Shape;
  $obj := $ObjId~toReg($objId);
  $shape := $ShapeField~toShape($shapeField);
  call $MASM^emit($MASM^Op~branchTestNotObjectShape($obj, $shape, $failure));
}

procedure $CacheIR~loadFixedSlotResult($objId: $ObjId, $slotField: $Int32Field, $output: $ValueReg)
  modifies $MASM^ops;
  modifies $MASM^emitPc;
{
  var $obj: $Reg;
  var $slot: $Int32;
  $obj := $ObjId~toReg($objId);
  $slot := $Int32Field~toInt32($slotField);
  call $MASM^emit($MASM^Op~loadObjectFixedSlot($obj, $slot, $output));
}

procedure $CacheIR~loadFixedSlot($objId: $ObjId, $slotField: $Int32Field, $valId: $ValId)
  modifies $MASM^ops;
  modifies $MASM^emitPc;
{
  var $obj: $Reg;
  var $slot: $Int32;
  var $val: $ValueReg;
  $obj := $ObjId~toReg($objId);
  $slot := $Int32Field~toInt32($slotField);
  $val := $ValId~toValueReg($valId);
  call $MASM^emit($MASM^Op~loadObjectFixedSlot($obj, $slot, $val));
}

procedure $CacheIR~guardToNumberResult($valId: $ValId, $output: $Reg, $failure: $MASM^Label)
  modifies $MASM^ops;
  modifies $MASM^emitPc;
{
  var $val: $ValueReg;
  var $done: $MASM^Label;
  var $notInt32: $MASM^Label;

  $val := $ValId~toValueReg($valId);

  call $done := $MASM^label();

  call $notInt32 := $MASM^label();
  call $MASM^emit($MASM^Op~branchTestNotInt32($val, $notInt32));
  call $MASM^emit($MASM^Op~unboxInt32($val, $output));
  call $MASM^emit($MASM^Op~jump($done));

  call $MASM^bind($notInt32);
  call $MASM^emit($MASM^Op~branchTestNotDouble($val, $failure));
  call $MASM^emit($MASM^Op~unboxDouble($val, $output));

  call $MASM^bind($done);
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

procedure $MASM^interpret()
  modifies heap;
  modifies valueRegs;
  modifies regs;
  modifies $MASM^pc;
{
  var op: $MASM^Op;
  op := $MASM^ops[$MASM^pc];

  // Using a jump table here instead of a long if...else chain speeds things up
  // a bit.

  // Assume that the op is something we recognize.
  assume
    is#$MASM^Op~jump(op) ||
    is#$MASM^Op~branchTestNotInt32(op) ||
    is#$MASM^Op~branchTestNotDouble(op) ||
    is#$MASM^Op~branchTestNotObject(op) ||
    is#$MASM^Op~branchTestNotObjectShape(op) ||
    is#$MASM^Op~unboxInt32(op) ||
    is#$MASM^Op~unboxDouble(op) ||
    is#$MASM^Op~unboxObject(op) ||
    is#$MASM^Op~loadObjectFixedSlot(op);

  // Branch depending on the opcode.
  goto
    interpret_$MASM^Op~jump,
    interpret_$MASM^Op~branchTestNotInt32,
    interpret_$MASM^Op~branchTestNotDouble,
    interpret_$MASM^Op~branchTestNotObject,
    interpret_$MASM^Op~branchTestNotObjectShape,
    interpret_$MASM^Op~unboxInt32,
    interpret_$MASM^Op~unboxDouble,
    interpret_$MASM^Op~unboxObject,
    interpret_$MASM^Op~loadObjectFixedSlot;

  // Each branch assumes that the op matches the opcode it's meant to handle,
  // and only that opcode.

  interpret_$MASM^Op~jump:
    assume {:partition}
      is#$MASM^Op~jump(op) &&
      !is#$MASM^Op~branchTestNotInt32(op) &&
      !is#$MASM^Op~branchTestNotDouble(op) &&
      !is#$MASM^Op~branchTestNotObject(op) &&
      !is#$MASM^Op~branchTestNotObjectShape(op) &&
      !is#$MASM^Op~unboxInt32(op) &&
      !is#$MASM^Op~unboxDouble(op) &&
      !is#$MASM^Op~unboxObject(op) &&
      !is#$MASM^Op~loadObjectFixedSlot(op);
    call $MASM~jump($label#$MASM^Op~jump(op));
    return;

  interpret_$MASM^Op~branchTestNotInt32:
    assume {:partition}
      !is#$MASM^Op~jump(op) &&
      is#$MASM^Op~branchTestNotInt32(op) &&
      !is#$MASM^Op~branchTestNotDouble(op) &&
      !is#$MASM^Op~branchTestNotObject(op) &&
      !is#$MASM^Op~branchTestNotObjectShape(op) &&
      !is#$MASM^Op~unboxInt32(op) &&
      !is#$MASM^Op~unboxDouble(op) &&
      !is#$MASM^Op~unboxObject(op) &&
      !is#$MASM^Op~loadObjectFixedSlot(op);
    call $MASM~branchTestNotInt32(
      $valueReg#$MASM^Op~branchTestNotInt32(op),
      $label#$MASM^Op~branchTestNotInt32(op)
    );
    return;

  interpret_$MASM^Op~branchTestNotDouble:
    assume {:partition}
      !is#$MASM^Op~jump(op) &&
      !is#$MASM^Op~branchTestNotInt32(op) &&
      is#$MASM^Op~branchTestNotDouble(op) &&
      !is#$MASM^Op~branchTestNotObject(op) &&
      !is#$MASM^Op~branchTestNotObjectShape(op) &&
      !is#$MASM^Op~unboxInt32(op) &&
      !is#$MASM^Op~unboxDouble(op) &&
      !is#$MASM^Op~unboxObject(op) &&
      !is#$MASM^Op~loadObjectFixedSlot(op);
    call $MASM~branchTestNotDouble(
      $valueReg#$MASM^Op~branchTestNotDouble(op),
      $label#$MASM^Op~branchTestNotDouble(op)
    );
    return;

  interpret_$MASM^Op~branchTestNotObject:
    assume {:partition}
      !is#$MASM^Op~jump(op) &&
      !is#$MASM^Op~branchTestNotInt32(op) &&
      !is#$MASM^Op~branchTestNotDouble(op) &&
      is#$MASM^Op~branchTestNotObject(op) &&
      !is#$MASM^Op~branchTestNotObjectShape(op) &&
      !is#$MASM^Op~unboxInt32(op) &&
      !is#$MASM^Op~unboxDouble(op) &&
      !is#$MASM^Op~unboxObject(op) &&
      !is#$MASM^Op~loadObjectFixedSlot(op);
    call $MASM~branchTestNotObject(
      $valueReg#$MASM^Op~branchTestNotObject(op),
      $label#$MASM^Op~branchTestNotObject(op)
    );
    return;

  interpret_$MASM^Op~branchTestNotObjectShape:
    assume {:partition}
      !is#$MASM^Op~jump(op) &&
      !is#$MASM^Op~branchTestNotInt32(op) &&
      !is#$MASM^Op~branchTestNotDouble(op) &&
      !is#$MASM^Op~branchTestNotObject(op) &&
      is#$MASM^Op~branchTestNotObjectShape(op) &&
      !is#$MASM^Op~unboxInt32(op) &&
      !is#$MASM^Op~unboxDouble(op) &&
      !is#$MASM^Op~unboxObject(op) &&
      !is#$MASM^Op~loadObjectFixedSlot(op);
    call $MASM~branchTestNotObjectShape(
      $objectReg#$MASM^Op~branchTestNotObjectShape(op),
      $shape#$MASM^Op~branchTestNotObjectShape(op),
      $label#$MASM^Op~branchTestNotObjectShape(op)
    );
    return;

  interpret_$MASM^Op~unboxInt32:
    assume {:partition}
      !is#$MASM^Op~jump(op) &&
      !is#$MASM^Op~branchTestNotInt32(op) &&
      !is#$MASM^Op~branchTestNotDouble(op) &&
      !is#$MASM^Op~branchTestNotObject(op) &&
      !is#$MASM^Op~branchTestNotObjectShape(op) &&
      is#$MASM^Op~unboxInt32(op) &&
      !is#$MASM^Op~unboxDouble(op) &&
      !is#$MASM^Op~unboxObject(op) &&
      !is#$MASM^Op~loadObjectFixedSlot(op);
    call $MASM~unboxInt32(
      $valueReg#$MASM^Op~unboxInt32(op),
      $int32Reg#$MASM^Op~unboxInt32(op)
    );
    return;

  interpret_$MASM^Op~unboxDouble:
    assume {:partition}
      !is#$MASM^Op~jump(op) &&
      !is#$MASM^Op~branchTestNotInt32(op) &&
      !is#$MASM^Op~branchTestNotDouble(op) &&
      !is#$MASM^Op~branchTestNotObject(op) &&
      !is#$MASM^Op~branchTestNotObjectShape(op) &&
      !is#$MASM^Op~unboxInt32(op) &&
      is#$MASM^Op~unboxDouble(op) &&
      !is#$MASM^Op~unboxObject(op) &&
      !is#$MASM^Op~loadObjectFixedSlot(op);
    call $MASM~unboxDouble(
      $valueReg#$MASM^Op~unboxDouble(op),
      $doubleReg#$MASM^Op~unboxDouble(op)
    );
    return;

  interpret_$MASM^Op~unboxObject:
    assume {:partition}
      !is#$MASM^Op~jump(op) &&
      !is#$MASM^Op~branchTestNotInt32(op) &&
      !is#$MASM^Op~branchTestNotDouble(op) &&
      !is#$MASM^Op~branchTestNotObject(op) &&
      !is#$MASM^Op~branchTestNotObjectShape(op) &&
      !is#$MASM^Op~unboxInt32(op) &&
      !is#$MASM^Op~unboxDouble(op) &&
      is#$MASM^Op~unboxObject(op) &&
      !is#$MASM^Op~loadObjectFixedSlot(op);
    call $MASM~unboxObject(
      $valueReg#$MASM^Op~unboxObject(op),
      $objectReg#$MASM^Op~unboxObject(op)
    );
    return;

  interpret_$MASM^Op~loadObjectFixedSlot:
    assume {:partition}
      !is#$MASM^Op~jump(op) &&
      !is#$MASM^Op~branchTestNotInt32(op) &&
      !is#$MASM^Op~branchTestNotDouble(op) &&
      !is#$MASM^Op~branchTestNotObject(op) &&
      !is#$MASM^Op~branchTestNotObjectShape(op) &&
      !is#$MASM^Op~unboxInt32(op) &&
      !is#$MASM^Op~unboxDouble(op) &&
      !is#$MASM^Op~unboxObject(op) &&
      is#$MASM^Op~loadObjectFixedSlot(op);
    call $MASM~loadObjectFixedSlot(
      $objectReg#$MASM^Op~loadObjectFixedSlot(op),
      $slot#$MASM^Op~loadObjectFixedSlot(op),
      $valueReg#$MASM^Op~loadObjectFixedSlot(op)
    );
    return;
}

procedure {:entrypoint} $GetProp($valId: $ValId, $objId: $ObjId, $scratchId: $ValId, $output: $Reg)
  modifies heap;
  modifies valueRegs;
  modifies regs;
  modifies $MASM^ops;
  modifies $MASM^emitPc;
  modifies $MASM^pc;
  modifies $MASM^nextLabel;
  modifies $MASM^labels;
{
  var failure: $MASM^Label;
  var endPc: $MASM^Pc;

  /* ... support code for extra test asserts ...
  var origVal: $Value;
  var origIsObject: $Bool;
  var origObj: $Object;
  var origOutput: $Value;

  call origVal := $ValueReg~getValue($ValId~toValueReg($valId));
  call origIsObject := $Value~isObject(origVal);
  if (origIsObject) {
    call origObj := $Value~toObject(origVal);
    if ($Object~shapeOf(heap, origObj) == $shape) {
      call origOutput := $Object~getFixedSlot(origObj, $slot);
    }
  }
  */

  $MASM^emitPc := 0;
  $MASM^nextLabel := 0;
  call failure := $MASM^label();
  call $CacheIR~guardToObject($valId, $objId, failure);
  call $CacheIR~guardShape($objId, $shapeField, failure);
  call $CacheIR~loadFixedSlot($objId, $slotField, $scratchId);
  call $CacheIR~guardToNumberResult($scratchId, $output, failure);
  assert $MASM^emitPc >= 0;

  endPc := $MASM^emitPc;
  call $MASM^emit($MASM^Op^noOp());

  call $MASM^bind(failure);
  call $MASM^emit($MASM^Op^noOp());

  $MASM^pc := 0;
  while ($MASM^pc < endPc) {
    call $MASM^interpret();
  }

  /* ... extra test asserts ...
  if ($MASM^pc == $MASM^labels[failure]) {
    return;
  }
  assert regs[$output] == origOutput;
  */
}
