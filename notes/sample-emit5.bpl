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

function $Object~shapeOf($object: $Object): $Shape;

procedure $Object~getProto($object: $Object)
  returns (ret: $Value)
{
  var $proto: $Value;
  var tmp'0: $Bool;
  var tmp'1: $Bool;
  var tmp'2: $Bool;
  $proto := $Object~getProtoUnchecked($object);
  call tmp'0 := $Value~isObject($proto);
  call tmp'1 := $Value~isNull($proto);
  call tmp'2 := $Value~isMagic($proto);
  assume tmp'0 || tmp'1 || tmp'2;
  ret := $proto;
  return;
}

function $Object~getProtoUnchecked($object: $Object): $Value;

procedure $Object~toNativeObject($object: $Object)
  returns (ret: $NativeObject)
{
  var $shape: $Shape;
  var $class: $Class;
  $shape := $Object~shapeOf($object);
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
  $shape := $Object~shapeOf($NativeObject^to$Object($nativeObject));
  assert $Shape~hasFixedSlot($shape, $slot);
  ret := $NativeObject~getFixedSlotUnchecked($nativeObject, $slot);
}

function $NativeObject~getFixedSlotUnchecked($nativeObject: $NativeObject, $slot: $Int32): $Value;

type $Shape;

function $Shape~classOf($shape: $Shape): $Class;

function $Shape~hasFixedSlot($shape: $Shape, $slot: $Int32): $Bool;

type $Class;

function $Class~isNativeObject($class: $Class): $Bool;

type $ValueReg;
type ValueRegs = [$ValueReg]$Value;

procedure $ValueReg~getValue(valueRegs: ValueRegs, $valueReg: $ValueReg)
  returns (ret: $Value)
{ 
  ret := valueRegs[$valueReg];
}

procedure $ValueReg~setValue(valueRegs: ValueRegs, $valueReg: $ValueReg, $value: $Value)
  returns (out_valueRegs: ValueRegs)
{
  out_valueRegs := valueRegs;
  out_valueRegs[$valueReg] := $value;
}

type $Reg;
type Regs = [$Reg]$Value;

procedure $Reg~getBoolean(regs: Regs, $reg: $Reg)
  returns (ret: $Bool)
{ 
  var tmp'0: $Bool;
  call tmp'0 := $Value~toBoolean(regs[$reg]);
  ret := tmp'0;
}

procedure $Reg~setBoolean(regs: Regs, $reg: $Reg, $boolean: $Bool)
  returns (out_regs: Regs)
{
  var tmp'0: $Value;
  call tmp'0 := $Value~fromBoolean($boolean);
  out_regs := regs;
  out_regs[$reg] := tmp'0;
}

procedure $Reg~getObject(regs: Regs, $reg: $Reg)
  returns (ret: $Object)
{ 
  var tmp'0: $Object;
  call tmp'0 := $Value~toObject(regs[$reg]);
  ret := tmp'0;
}

procedure $Reg~setObject(regs: Regs, $reg: $Reg, $object: $Object)
  returns (out_regs: Regs)
{
  var tmp'0: $Value;
  call tmp'0 := $Value~fromObject($object);
  out_regs := regs;
  out_regs[$reg] := tmp'0;
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

type $MASM^Bind = int;

var $MASM^pcBinds: [$MASM^Pc]$MASM^Bind;

procedure {:inline 1} $MASM^bind(bind: $MASM^Bind, label: $MASM^Label)
  modifies $MASM^labels;
{
  $MASM^pcBinds[$MASM^emitPc] := bind;
  $MASM^labels[label] := $MASM^emitPc;
}

procedure {:inline 1} $MASM^jump(label: $MASM^Label)
  modifies $MASM^pc;
{
  $MASM^pc := $MASM^labels[label];
}

var $MASM^emitPc: $MASM^Pc;

var $MASM^ops: [$MASM^Pc]$MASM^Op;

procedure $MASM^emit(op: $MASM^Op)
  modifies $MASM^ops;
  modifies $MASM^emitPc;
{
  $MASM^ops[$MASM^emitPc] := op;
  $MASM^emitPc := $MASM^emitPc + 1;
}

function {:constructor} $MASM^Op~jump($label: $MASM^Label): $MASM^Op;

function {:constructor} $MASM^Op~storeBoolean($boolean: $Bool, $dstReg: $Reg): $MASM^Op;

procedure $MASM~storeBoolean(regs: Regs, $boolean: $Bool, $dstReg: $Reg)
  returns (out_regs: Regs)
  modifies $MASM^pc;
{
  call out_regs := $Reg~setBoolean(regs, $dstReg, $boolean);
  call $MASM^step();
}

function {:constructor} $MASM^Op~branchTestNull($valueReg: $ValueReg, $label: $MASM^Label): $MASM^Op;

function {:constructor} $MASM^Op~branchTestMagic($valueReg: $ValueReg, $label: $MASM^Label): $MASM^Op;

function {:constructor} $MASM^Op~branchTestNotObject($valueReg: $ValueReg, $label: $MASM^Label): $MASM^Op;

function {:constructor} $MASM^Op~unboxObject($valueReg: $ValueReg, $objectReg: $Reg): $MASM^Op;

procedure $MASM~unboxObject(valueRegs: ValueRegs, regs: Regs, $valueReg: $ValueReg, $objectReg: $Reg)
  returns (out_regs: Regs)
  modifies $MASM^pc;
{
  var $value: $Value;
  var tmp'0: $Value;
  var tmp'1: $Object;
  call tmp'0 := $ValueReg~getValue(valueRegs, $valueReg);
  $value := tmp'0;
  call tmp'1 := $Value~toObject($value);
  call out_regs := $Reg~setObject(regs, $objectReg, tmp'1);
  call $MASM^step();
}

function {:constructor} $MASM^Op~branchTestObjectEq($lhsReg: $Reg, $rhsReg: $Reg, $label: $MASM^Label): $MASM^Op;

function {:constructor} $MASM^Op~loadObjectProto($objectReg: $Reg, $protoReg: $ValueReg): $MASM^Op;

procedure $MASM~loadObjectProto(valueRegs: ValueRegs, regs: Regs, $objectReg: $Reg, $protoReg: $ValueReg)
  returns (out_valueRegs: ValueRegs)
  modifies $MASM^pc;
{
  var $object: $Object;
  var tmp'0: $Object;
  var tmp'1: $Value;
  call tmp'0 := $Reg~getObject(regs, $objectReg);
  $object := tmp'0;
  call tmp'1 := $Object~getProto($object);
  call out_valueRegs := $ValueReg~setValue(valueRegs, $protoReg, tmp'1);
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

type $CacheIR^Branch = int;

var $CacheIR^trace: [$CacheIR^Branch, $MASM^Pc]bool;

procedure {:inline 1} $CacheIR^record(branch: $CacheIR^Branch, cond: bool)
  modifies $CacheIR^trace;
{
  $CacheIR^trace[branch, $MASM^emitPc] := cond;
}

procedure $CacheIR~loadInstanceOfObjectResult(valueRegs: ValueRegs, $lhsId: $ValId, $hasKnownLhs: $Bool, $protoId: $ObjId, $scratch: $Reg, $output: $Reg, $failure: $MASM^Label)
  modifies $MASM^emitPc;
  modifies $MASM^ops;
  modifies $CacheIR^trace;
{
  var $lhs: $ValueReg;
  var $knownLhs: $Value;
  var $hasKnownObjectLhs: $Bool;
  var $proto: $Reg;
  var $loop: $MASM^Label;
  var $returnFalse: $MASM^Label;
  var $returnTrue: $MASM^Label;
  var $done: $MASM^Label;
  var i: int;

  $lhs := $ValId~toValueReg($lhsId);
  $proto := $ObjId~toReg($protoId);

  call $loop := $MASM^label();
  call $returnFalse := $MASM^label();
  call $returnTrue := $MASM^label();
  call $done := $MASM^label();

  $hasKnownObjectLhs := false;
  if ($hasKnownLhs) {
    call $knownLhs := $ValueReg~getValue(valueRegs, $lhs);
    call $hasKnownObjectLhs := $Value~isObject($knownLhs);
  }

  if (!$hasKnownObjectLhs) {
    call $CacheIR^record(0, true);
    i := 0;
    while (i < 1) {
      call $CacheIR^record(1, true);
      call $MASM^emit($MASM^Op~branchTestNotObject($lhs, $returnFalse));
      i := i + 1;
    }
    call $CacheIR^record(1, false);
  } else {
    call $CacheIR^record(0, false);
  }
  call $MASM^emit($MASM^Op~unboxObject($lhs, $scratch));
  call $MASM^emit($MASM^Op~loadObjectProto($scratch, $lhs));

  call $MASM^bind(1, $loop);
  call $MASM^emit($MASM^Op~branchTestNull($lhs, $returnFalse));
  call $MASM^emit($MASM^Op~branchTestMagic($lhs, $failure));

  call $MASM^emit($MASM^Op~branchTestNotObject($lhs, $returnFalse));
  call $MASM^emit($MASM^Op~unboxObject($lhs, $scratch));
  call $MASM^emit($MASM^Op~branchTestObjectEq($scratch, $proto, $returnTrue));

  call $MASM^emit($MASM^Op~loadObjectProto($scratch, $lhs));
  call $MASM^emit($MASM^Op~jump($loop));

  call $MASM^bind(2, $returnFalse);
  call $MASM^emit($MASM^Op~storeBoolean(false, $output));
  call $MASM^emit($MASM^Op~jump($done));

  call $MASM^bind(3, $returnTrue);
  call $MASM^emit($MASM^Op~storeBoolean(true, $output));

  call $MASM^bind(4, $done);
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

procedure {:entrypoint} $InstanceOf($lhsId: $ValId, $hasKnownLhs: $Bool, $protoId: $ObjId, $scratch: $Reg, $output: $Reg)
  modifies $MASM^emitPc;
  modifies $MASM^ops;
  modifies $CacheIR^trace;
{
  var op: $MASM^Op;

  var valueRegs: ValueRegs;
  var regs: Regs;

  var $failure: $MASM^Label;

  var $lhs: $ValueReg;
  var $proto: $Reg;
  var proto: $Object;

  var $MASM~branchTestNull'$value: $Value;
  var $MASM~branchTestNull'tmp'0: $Value;
  var $MASM~branchTestNull'tmp'1: $Bool;

  var $MASM~branchTestMagic'$value: $Value;
  var $MASM~branchTestMagic'tmp'0: $Value;
  var $MASM~branchTestMagic'tmp'1: $Bool;

  var $MASM~branchTestNotObject'$value: $Value;
  var $MASM~branchTestNotObject'tmp'0: $Value;
  var $MASM~branchTestNotObject'tmp'1: $Bool;

  var $MASM~branchTestObjectEq'$lhs: $Object;
  var $MASM~branchTestObjectEq'$rhs: $Object;
  var $MASM~branchTestObjectEq'tmp'0: $Object;
  var $MASM~branchTestObjectEq'tmp'1: $Object;

  $lhs := $ValId~toValueReg($lhsId);
  $proto := $ObjId~toReg($protoId);

  assume $proto != $scratch;
  assume $Value~typeOf(regs[$proto]) == $ValueType~Object();

  call proto := $Reg~getObject(regs, $proto);

  $MASM^emitPc := 0;

  assume (forall pc: $MASM^Pc :: $MASM^pcBinds[pc] == -1);
  //assume (forall branch: $CacheIR^Branch, pc: $MASM^Pc :: !$CacheIR^trace[branch, pc]);

  call $failure := $MASM^label();

  call $CacheIR~loadInstanceOfObjectResult(valueRegs, $lhsId, $hasKnownLhs, $protoId, $scratch, $output, $failure);

  $MASM^emitPc := $MASM^emitPc + 1;

  call $MASM^bind(0, $failure);
  $MASM^emitPc := $MASM^emitPc + 1;

  assert $MASM^emitPc >= 0;
  assume (forall pc: $MASM^Pc :: $MASM^pcBinds[pc] >= -1 && $MASM^pcBinds[pc] <= 4);

  $MASM^pc := 0;

  if ($CacheIR^trace[0, $MASM^pc]) {
    while ($CacheIR^trace[1, $MASM^pc]) {
      op'0$MASM~branchTestNotObject:
        op := $MASM^ops[$MASM^pc];
        call $MASM~branchTestNotObject'tmp'0 := $ValueReg~getValue(valueRegs, $valueReg#$MASM^Op~branchTestNotObject(op));
        $MASM~branchTestNotObject'$value := $MASM~branchTestNotObject'tmp'0;
        call $MASM~branchTestNotObject'tmp'1 := $Value~isObject($MASM~branchTestNotObject'$value);
        if (!$MASM~branchTestNotObject'tmp'1) {
          call $MASM^jump($label#$MASM^Op~branchTestNotObject(op));
          goto label$returnFalse;
        }
        call $MASM^step();
    }
  }

  op'1$MASM~unboxObject:
    op := $MASM^ops[$MASM^pc];
    call regs := $MASM~unboxObject(
      valueRegs,
      regs,
      $valueReg#$MASM^Op~unboxObject(op),
      $objectReg#$MASM^Op~unboxObject(op)
    );

  op'2$MASM~loadObjectProto:
    op := $MASM^ops[$MASM^pc];
    call valueRegs := $MASM~loadObjectProto(
      valueRegs,
      regs,
      $objectReg#$MASM^Op~loadObjectProto(op),
      $protoReg#$MASM^Op~loadObjectProto(op)
    );

  assert regs[$proto] == $Value~fromObjectUnchecked(proto);

  label$loop:
    assume $MASM^pcBinds[$MASM^pc] == 1;
    assume regs[$proto] == $Value~fromObjectUnchecked(proto);

  op'3$MASM^branchTestNull:
    op := $MASM^ops[$MASM^pc];
    call $MASM~branchTestNull'tmp'0 := $ValueReg~getValue(valueRegs, $valueReg#$MASM^Op~branchTestNull(op));
    $MASM~branchTestNull'$value := $MASM~branchTestNull'tmp'0;
    call $MASM~branchTestNull'tmp'1 := $Value~isNull($MASM~branchTestNull'$value);
    if ($MASM~branchTestNull'tmp'1) {
      call $MASM^jump($label#$MASM^Op~branchTestNull(op));
      goto label$returnFalse;
    }
    call $MASM^step();

  op'4$MASM~branchTestMagic:
    op := $MASM^ops[$MASM^pc];
    call $MASM~branchTestMagic'tmp'0 := $ValueReg~getValue(valueRegs, $valueReg#$MASM^Op~branchTestMagic(op));
    $MASM~branchTestMagic'$value := $MASM~branchTestMagic'tmp'0;
    call $MASM~branchTestMagic'tmp'1 := $Value~isMagic($MASM~branchTestMagic'$value);
    if ($MASM~branchTestMagic'tmp'1) {
      call $MASM^jump($label#$MASM^Op~branchTestMagic(op));
      goto label$failure;
    }
    call $MASM^step();

  op'5$MASM~branchTestNotObject:
    op := $MASM^ops[$MASM^pc];
    call $MASM~branchTestNotObject'tmp'0 := $ValueReg~getValue(valueRegs, $valueReg#$MASM^Op~branchTestNotObject(op));
    $MASM~branchTestNotObject'$value := $MASM~branchTestNotObject'tmp'0;
    call $MASM~branchTestNotObject'tmp'1 := $Value~isObject($MASM~branchTestNotObject'$value);
    if (!$MASM~branchTestNotObject'tmp'1) {
      call $MASM^jump($label#$MASM^Op~branchTestNotObject(op));
      goto label$returnFalse;
    }
    call $MASM^step();

  op'6$MASM~unboxObject:
    op := $MASM^ops[$MASM^pc];
    call regs := $MASM~unboxObject(
      valueRegs,
      regs,
      $valueReg#$MASM^Op~unboxObject(op),
      $objectReg#$MASM^Op~unboxObject(op)
    );

  op'7$MASM~branchTestObjectEq:
    op := $MASM^ops[$MASM^pc];
    call $MASM~branchTestObjectEq'tmp'0 := $Reg~getObject(regs, $lhsReg#$MASM^Op~branchTestObjectEq(op));
    $MASM~branchTestObjectEq'$lhs := $MASM~branchTestObjectEq'tmp'0;
    call $MASM~branchTestObjectEq'tmp'1 := $Reg~getObject(regs, $rhsReg#$MASM^Op~branchTestObjectEq(op));
    $MASM~branchTestObjectEq'$rhs := $MASM~branchTestObjectEq'tmp'1;
    if ($MASM~branchTestObjectEq'$lhs == $MASM~branchTestObjectEq'$rhs) {
      call $MASM^jump($label#$MASM^Op~branchTestObjectEq(op));
      goto label$returnTrue;
    }
    call $MASM^step();

  op'8$MASM~loadObjectProto:
    op := $MASM^ops[$MASM^pc];
    call valueRegs := $MASM~loadObjectProto(
      valueRegs,
      regs,
      $objectReg#$MASM^Op~loadObjectProto(op),
      $protoReg#$MASM^Op~loadObjectProto(op)
    );

  op'9$MASM~jump:
    op := $MASM^ops[$MASM^pc];
    call $MASM^jump($label#$MASM^Op~jump(op));
    assert regs[$proto] == $Value~fromObjectUnchecked(proto);
    goto label$loop;

  label$returnFalse:
    assume $MASM^pcBinds[$MASM^pc] == 2;

  op'10$MASM~storeBoolean:
    op := $MASM^ops[$MASM^pc];
    call regs := $MASM~storeBoolean(
      regs,
      $boolean#$MASM^Op~storeBoolean(op),
      $dstReg#$MASM^Op~storeBoolean(op)
    );

  op'11$MASM~jump:
    op := $MASM^ops[$MASM^pc];
    call $MASM^jump($label#$MASM^Op~jump(op));
    goto label$done;

  label$returnTrue:
    assume $MASM^pcBinds[$MASM^pc] == 3;

  op'12$MASM~storeBoolean:
    op := $MASM^ops[$MASM^pc];
    call regs := $MASM~storeBoolean(
      regs,
      $boolean#$MASM^Op~storeBoolean(op),
      $dstReg#$MASM^Op~storeBoolean(op)
    );

  label$done:
    assume $MASM^pcBinds[$MASM^pc] == 4;

    return;

  label$failure:
    assume $MASM^pcBinds[$MASM^pc] == 0;
}
