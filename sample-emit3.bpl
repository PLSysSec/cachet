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

procedure $Value~isBoolean($value: $Value)
  returns (ret: $Bool)
{
  ret := $Value~typeOf($value) == $ValueType~Boolean();
}

procedure $Value~fromBoolean($boolean: $Bool)
  returns (ret: $Value)
{
  var $value: $Value;
  var tmp'0: $Bool;
  $value := $Value~fromBooleanUnchecked($boolean);
  call tmp'0 := $Value~isBoolean($value);
  assume tmp'0;
  assume $Value~toBooleanUnchecked($value) == $boolean;
  ret := $value;
}

function $Value~fromBooleanUnchecked($boolean: $Bool): $Value;

procedure $Value~toBoolean($value: $Value)
  returns (ret: $Bool)
{
  var $boolean: $Bool;
  var tmp'0: $Bool;
  call tmp'0 := $Value~isBoolean($value);
  assert tmp'0;
  $boolean := $Value~toBooleanUnchecked($value);
  assume $Value~fromBooleanUnchecked($boolean) == $value;
  ret := $boolean;
}

function $Value~toBooleanUnchecked($value: $Value): $Bool;

procedure $Value~isUndefined($value: $Value)
  returns (ret: $Bool)
{
  ret := $Value~typeOf($value) == $ValueType~Undefined();
}

procedure $Value~isNull($value: $Value)
  returns (ret: $Bool)
{
  ret := $Value~typeOf($value) == $ValueType~Null();
}

procedure $Value~isMagic($value: $Value)
  returns (ret: $Bool)
{
  ret := $Value~typeOf($value) == $ValueType~Magic();
}

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

procedure $Object~getProto($object: $Object)
  returns (ret: $Value)
{
  var $proto: $Value;
  var tmp'0: $Bool;
  var tmp'1: $Bool;
  var tmp'2: $Bool;
  $proto := $Object~getProtoUnchecked(heap, $object);
  call tmp'0 := $Value~isObject($proto);
  call tmp'1 := $Value~isNull($proto);
  call tmp'2 := $Value~isMagic($proto);
  assume tmp'0 || tmp'1 || tmp'2;
  ret := $proto;
  return;
}

function $Object~getProtoUnchecked(heap: Heap, $object: $Object): $Value;

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
//function {:constructor} $MASM^Op^noOp(): $MASM^Op;

type $MASM^Pc = int;

var $MASM^pc: $MASM^Pc;

procedure {:inline 1} $MASM^step()
  modifies $MASM^pc;
{
  $MASM^pc := $MASM^pc + 1;
}

type $MASM^Bind = int;

type {:datatype} $MASM^ControlFlow;
function {:constructor} $MASM^Seq(): $MASM^ControlFlow;
function {:constructor} $MASM^Jump(bind: $MASM^Bind): $MASM^ControlFlow;

var $MASM^controlFlow: $MASM^ControlFlow;

type $MASM^Label = int;

var $MASM^nextLabel: $MASM^Label;

procedure $MASM^label()
  returns (label: $MASM^Label)
  modifies $MASM^nextLabel;
{
  label := $MASM^nextLabel;
  $MASM^nextLabel := $MASM^nextLabel + 1;
}

type {:datatype} $MASM^BoundLabel;
function {:constructor} $MASM^UnboundLabel(): $MASM^BoundLabel;
function {:constructor} $MASM^BoundLabel(bind: $MASM^Bind, pc: $MASM^Pc): $MASM^BoundLabel;

var $MASM^boundLabels: [$MASM^Label]$MASM^BoundLabel;

procedure $MASM^bind(bind: $MASM^Bind, label: $MASM^Label)
  modifies $MASM^boundLabels;
{
  $MASM^boundLabels[label] := $MASM^BoundLabel(bind, $MASM^emitPc);
}

procedure {:inline 1} $MASM^jump(label: $MASM^Label)
  modifies $MASM^pc;
  modifies $MASM^controlFlow;
{
  var boundLabel: $MASM^BoundLabel;
  boundLabel := $MASM^boundLabels[label];
  assert is#$MASM^BoundLabel(boundLabel);
  $MASM^controlFlow := $MASM^Jump(bind#$MASM^BoundLabel(boundLabel));
  $MASM^pc := pc#$MASM^BoundLabel(boundLabel);
}

var $MASM^emitPc: $MASM^Pc;
var $MASM^emitOps: [$MASM^Pc]$MASM^Op;
var $MASM^emitTrace: [$MASM^Pc]bool;

procedure $MASM^emit(op: $MASM^Op)
  modifies $MASM^emitOps;
  modifies $MASM^emitPc;
{
  $MASM^emitOps[$MASM^emitPc] := op;
  $MASM^emitPc := $MASM^emitPc + 1;
}

procedure $MASM^record(branch: bool)
  modifies $MASM^emitTrace;
{
  $MASM^emitTrace[$MASM^emitPc] := branch;
}

function {:constructor} $MASM^Op~jump($label: $MASM^Label): $MASM^Op;

procedure $MASM~jump($label: $MASM^Label)
  modifies $MASM^pc;
{
  call $MASM^jump($label);
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

function {:constructor} $MASM^Op~loadObjectFixedSlot($objectReg: $Reg, $slot: $Int32, $valueReg: $ValueReg): $MASM^Op;

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
  modifies $MASM^emitPc;
  modifies $MASM^emitOps;
{
  var $val: $ValueReg;
  var $obj: $Reg;
  $val := $ValId~toValueReg($valId);
  $obj := $ObjId~toReg($objId);
  call $MASM^emit($MASM^Op~branchTestNotObject($val, $failure));
  call $MASM^emit($MASM^Op~unboxObject($val, $obj));
}

procedure $CacheIR~guardToNumberResult($valId: $ValId, $output: $Reg, $failure: $MASM^Label)
  modifies $MASM^nextLabel;
  modifies $MASM^boundLabels;
  modifies $MASM^emitPc;
  modifies $MASM^emitOps;
{
  var $val: $ValueReg;
  var $done: $MASM^Label;
  var $notDouble: $MASM^Label;

  $val := $ValId~toValueReg($valId);

  call $done := $MASM^label();

  call $notDouble := $MASM^label();
  call $MASM^emit($MASM^Op~branchTestNotDouble($val, $notDouble));
  call $MASM^emit($MASM^Op~unboxDouble($val, $output));
  call $MASM^emit($MASM^Op~jump($done));

  call $MASM^bind(1, $notDouble);
  call $MASM^emit($MASM^Op~branchTestNotInt32($val, $failure));
  call $MASM^emit($MASM^Op~unboxInt32($val, $output));

  call $MASM^bind(2, $done);
}

procedure $CacheIR~guardShape($objId: $ObjId, $shapeField: $ShapeField, $failure: $MASM^Label)
  modifies $MASM^emitPc;
  modifies $MASM^emitOps;
{
  var $obj: $Reg;
  var $shape: $Shape;
  $obj := $ObjId~toReg($objId);
  $shape := $ShapeField~toShape($shapeField);
  call $MASM^emit($MASM^Op~branchTestNotObjectShape($obj, $shape, $failure));
}

procedure $CacheIR~loadFixedSlotResult($objId: $ObjId, $slotField: $Int32Field, $output: $ValueReg)
  modifies $MASM^emitPc;
  modifies $MASM^emitOps;
{
  var $obj: $Reg;
  var $slot: $Int32;
  $obj := $ObjId~toReg($objId);
  $slot := $Int32Field~toInt32($slotField);
  call $MASM^emit($MASM^Op~loadObjectFixedSlot($obj, $slot, $output));
}

procedure $CacheIR~loadFixedSlot($objId: $ObjId, $slotField: $Int32Field, $valId: $ValId)
  modifies $MASM^emitPc;
  modifies $MASM^emitOps;
{
  var $obj: $Reg;
  var $slot: $Int32;
  var $val: $ValueReg;
  $obj := $ObjId~toReg($objId);
  $slot := $Int32Field~toInt32($slotField);
  $val := $ValId~toValueReg($valId);
  call $MASM^emit($MASM^Op~loadObjectFixedSlot($obj, $slot, $val));
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

procedure {:entrypoint} $GetProp($valId: $ValId, $objId: $ObjId, $scratchId: $ValId, $output: $Reg)
  modifies heap;
  modifies valueRegs;
  modifies regs;
  modifies $MASM^nextLabel;
  modifies $MASM^boundLabels;
  modifies $MASM^emitPc;
  modifies $MASM^emitOps;
  modifies $MASM^emitTrace;
  modifies $MASM^pc;
{
  var failure: $MASM^Label;
  var op: $MASM^Op;

  assume (forall label: $MASM^Label :: $MASM^boundLabels[label] == $MASM^UnboundLabel());
  //assume (forall pc: $MASM^Pc :: $MASM^emitOps[pc] == $MASM^Op^noOp());

  $MASM^nextLabel := 0;
  $MASM^emitPc := 0;

  call failure := $MASM^label();

  call $CacheIR~guardToObject($valId, $objId, failure);
  call $CacheIR~guardShape($objId, $shapeField, failure);
  call $CacheIR~loadFixedSlot($objId, $slotField, $scratchId);
  call $CacheIR~guardToNumberResult($scratchId, $output, failure);

  $MASM^emitPc := $MASM^emitPc + 1;

  call $MASM^bind(0, failure);
  $MASM^emitPc := $MASM^emitPc + 1;

  assert $MASM^emitPc >= 0;

  $MASM^pc := 0;
  $MASM^controlFlow := $MASM^Seq();

  op := $MASM^emitOps[$MASM^pc];
  call $MASM~branchTestNotObject(
    $valueReg#$MASM^Op~branchTestNotObject(op),
    $label#$MASM^Op~branchTestNotObject(op)
  );
  goto seq'0, label$notDouble, label$done, label$failure;

  seq'0:
    assume {:partition} $MASM^controlFlow == $MASM^Seq();

    op := $MASM^emitOps[$MASM^pc];
    call $MASM~unboxObject(
      $valueReg#$MASM^Op~unboxObject(op),
      $objectReg#$MASM^Op~unboxObject(op)
    );

    op := $MASM^emitOps[$MASM^pc];
    call $MASM~branchTestNotObjectShape(
      $objectReg#$MASM^Op~branchTestNotObjectShape(op),
      $shape#$MASM^Op~branchTestNotObjectShape(op),
      $label#$MASM^Op~branchTestNotObjectShape(op)
    );
    goto seq'1, label$notDouble, label$done, label$failure;

  seq'1:
    assume {:partition} $MASM^controlFlow == $MASM^Seq();

    op := $MASM^emitOps[$MASM^pc];
    call $MASM~loadObjectFixedSlot(
      $objectReg#$MASM^Op~loadObjectFixedSlot(op),
      $slot#$MASM^Op~loadObjectFixedSlot(op),
      $valueReg#$MASM^Op~loadObjectFixedSlot(op)
    );

    op := $MASM^emitOps[$MASM^pc];
    call $MASM~branchTestNotDouble(
      $valueReg#$MASM^Op~branchTestNotDouble(op),
      $label#$MASM^Op~branchTestNotDouble(op)
    );
    goto seq'2, label$notDouble, label$done, label$failure;

  seq'2:
    assume {:partition} $MASM^controlFlow == $MASM^Seq();

    op := $MASM^emitOps[$MASM^pc];
    call $MASM~unboxDouble(
      $valueReg#$MASM^Op~unboxDouble(op),
      $doubleReg#$MASM^Op~unboxDouble(op)
    );

    op := $MASM^emitOps[$MASM^pc];
    call $MASM~jump($label#$MASM^Op~jump(op));
    goto label$notDouble, label$done, label$failure;

  label$notDouble:
    assume {:partition} $MASM^controlFlow == $MASM^Jump(1);
    $MASM^controlFlow := $MASM^Seq();

    op := $MASM^emitOps[$MASM^pc];
    call $MASM~branchTestNotInt32(
      $valueReg#$MASM^Op~branchTestNotInt32(op),
      $label#$MASM^Op~branchTestNotInt32(op)
    );
    goto seq'3, label$notDouble, label$done, label$failure;

  seq'3:
    assume {:partition} $MASM^controlFlow == $MASM^Seq();

    op := $MASM^emitOps[$MASM^pc];
    call $MASM~unboxInt32(
      $valueReg#$MASM^Op~unboxInt32(op),
      $int32Reg#$MASM^Op~unboxInt32(op)
    );
    goto seq'4;

  label$done:
    assume {:partition} $MASM^controlFlow == $MASM^Jump(2);
    $MASM^controlFlow := $MASM^Seq();

  seq'4:
    return;

  label$failure:
    assume {:partition} $MASM^controlFlow == $MASM^Jump(0);
    $MASM^controlFlow := $MASM^Seq();
}
