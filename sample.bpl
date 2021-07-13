type Heap;
var heap: Heap;
procedure ObserveHeapWrite();
  modifies heap;

var bail: bool;

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

procedure $Value~toObject($value: $Value)
  returns (ret: $Object)
  modifies bail;
{
  var tmp'0: $Bool;
  call tmp'0 := $Value~isObject($value);
  if (!tmp'0) {
    bail := true;
    return;
  }
  ret := $Value~toObjectUnchecked($value);
  return;
}

function $Value~toObjectUnchecked($value: $Value): $Object;

type $Object;

function $Object~shapeOf(heap: Heap, $object: $Object): $Shape;

procedure $Object~toNativeObject($object: $Object)
  returns (ret: $NativeObject)
  modifies bail;
{
  var $shape: $Shape;
  var $class: $Class;
  $shape := $Object~shapeOf(heap, $object);
  $class := $Shape~classOf($shape);
  if (!$Class~isNativeObject($class)) {
    bail := true;
    return;
  }
  ret := $NativeObject^from$Object($object);
  return;
}

procedure $Object~getFixedSlot($object: $Object, $slot: $Int32)
  returns (ret: $Value)
  modifies bail;
{
  var $nativeObject: $NativeObject;
  var tmp'0: $NativeObject;
  var tmp'1: $Value;
  call tmp'0 := $Object~toNativeObject($object);
  if (bail) {
    return;
  }
  $nativeObject := tmp'0;
  call tmp'1 := $NativeObject~getFixedSlot($nativeObject, $slot);
  if (bail) {
    return;
  }
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
  modifies bail;
{
  var $shape: $Shape;
  $shape := $Object~shapeOf(heap, $NativeObject^to$Object($nativeObject));
  if (!$Shape~hasFixedSlot($shape, $slot)) {
    bail := true;
    return;
  }
  ret := $NativeObject~getFixedSlotUnchecked(heap, $nativeObject, $slot);
  return;
}

function $NativeObject~getFixedSlotUnchecked(heap: Heap, $nativeObject: $NativeObject, $slot: $Int32): $Value;

type $Shape;

function $Shape~classOf($shape: $Shape): $Class;

function $Shape~hasFixedSlot($shape: $Shape, $slot: $Int32): $Bool;

type $Class;

function $Class~isNativeObject($class: $Class): $Bool;

procedure $GuardToObject($value: $Value)
  returns ($object: $Object)
  modifies bail;
{
  var tmp'0: $Object;
  call tmp'0 := $Value~toObject($value);
  if (bail) {
    return;
  }
  $object := tmp'0;
  return;
}

procedure $GuardShape($object: $Object, $shape: $Shape)
  modifies bail;
{
  if (!($Object~shapeOf(heap, $object) == $shape)) {
    bail := true;
    return;
  }
  return;
}

procedure $LoadFixedSlotResult($object: $Object, $slot: $Int32)
  returns (ret: $Value)
  modifies bail;
{
  var tmp'0: $Value;
  call tmp'0 := $Object~getFixedSlot($object, $slot);
  assert !bail;
  ret := tmp'0;
  return;
}

const $shape: $Shape;
const $class: $Class;
const $slot: $Int32;

axiom $Shape~hasFixedSlot($shape, $slot);
axiom $Shape~classOf($shape) == $class;
axiom $Class~isNativeObject($class);

procedure $GetProp($input: $Value)
  returns (output: $Value)
  modifies heap;
  modifies bail;
{
  var $input'0: $Object;
  var again: $Value;
  call $input'0 := $GuardToObject($input);
  assume !bail;
  call $GuardShape($input'0, $shape);
  assume !bail;
  call output := $LoadFixedSlotResult($input'0, $slot);
  //call ObserveHeapWrite();
  call again := $LoadFixedSlotResult($input'0, $slot);
  assert output == again;
  return;
}
