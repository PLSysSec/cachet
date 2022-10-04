type #ValueReg = #Reg;

procedure #ValueReg~scratchReg($valueReg: #ValueReg)
  returns (reg: #Reg)
{
  reg := $valueReg;
}

procedure #ValueReg~fromReg($reg: #Reg)
  returns (valueReg: #ValueReg)
{
  valueReg := $reg;
}

var #MASM~regs: #Map #Reg #RegData;

procedure #MASM~getData($reg: #Reg)
  returns (ret: #RegData)
{
  ret := #Map~get(#MASM~regs, $reg);
}

procedure #MASM~setData($reg: #Reg, $data: #RegData)
  modifies #MASM~regs;
{
  #MASM~regs := #Map~set(#MASM~regs, $reg, $data);
}
    
procedure #MASM~getValue($valueReg: #ValueReg)
  returns (ret: #Value)
{
  var tmp'0: #RegData;
  call tmp'0 := #MASM~getData($valueReg);
  call ret := #RegData~toValue(tmp'0);
}

procedure #MASM~setValue($valueReg: #ValueReg, $value: #Value)
  modifies #MASM~regs;
{
  var tmp'0: #RegData;
  call tmp'0 := #RegData~fromValue($value);
  call #MASM~setData($valueReg, tmp'0);
}

procedure #MASM~getUnboxedValue($reg: #Reg)
  returns (ret: #Value)
{
  var tmp'0: #RegData;
  call tmp'0 := #MASM~getData($reg);
  call ret := #RegData~toUnboxedValue(tmp'0);
}

procedure #MASM~setUnboxedValue($reg: #Reg, $value: #Value)
  modifies #MASM~regs;
{
  var tmp'0: #RegData;
  call tmp'0 := #RegData~fromUnboxedValue($value);
  call #MASM~setData($reg, tmp'0);
}

procedure #MASM~getInt32($reg: #Reg)
  returns (ret: #Int32)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getUnboxedValue($reg);
  call ret := #Value~toInt32(tmp'0);
}

procedure #MASM~setInt32($reg: #Reg, $int32: #Int32)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromInt32($int32);
  call #MASM~setUnboxedValue($reg, tmp'0);
}

procedure #MASM~getBool($reg: #Reg)
  returns (ret: #Bool)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getUnboxedValue($reg);
  call ret := #Value~toBool(tmp'0);
}

procedure #MASM~setBool($reg: #Reg, $bool: #Bool)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromBool($bool);
  call #MASM~setUnboxedValue($reg, tmp'0);
}

procedure #MASM~getObject($reg: #Reg)
  returns (ret: #Object)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getUnboxedValue($reg);
  call ret := #Value~toObject(tmp'0);
}

procedure #MASM~setObject($reg: #Reg, $object: #Object)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromObject($object);
  call #MASM~setUnboxedValue($reg, tmp'0);
}

procedure #MASM~getString($reg: #Reg)
  returns (ret: #String)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getUnboxedValue($reg);
  call ret := #Value~toString(tmp'0);
}

procedure #MASM~setString($reg: #Reg, $string: #String)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromString($string);
  call #MASM~setUnboxedValue($reg, tmp'0);
}

procedure #MASM~getSymbol($reg: #Reg)
  returns (ret: #Symbol)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getUnboxedValue($reg);
  call ret := #Value~toSymbol(tmp'0);
}

procedure #MASM~setSymbol($reg: #Reg, $symbol: #Symbol)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromSymbol($symbol);
  call #MASM~setUnboxedValue($reg, tmp'0);
}

procedure #MASM~getBigInt($reg: #Reg)
  returns (ret: #BigInt)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getUnboxedValue($reg);
  call ret := #Value~toBigInt(tmp'0);
}

procedure #MASM~setBigInt($reg: #Reg, $bigInt: #BigInt)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromBigInt($bigInt);
  call #MASM~setUnboxedValue($reg, tmp'0);
}

var #MASM~floatRegs: #Map #FloatReg #Double;

procedure #MASM~getDouble($floatReg: #FloatReg)
  returns (ret: #Double)
{
  ret := #Map~get(#MASM~floatRegs, $floatReg);
}

procedure #MASM~setDouble($floatReg: #FloatReg, $double: #Double)
  modifies #MASM~regs;
{
    #MASM~floatRegs := #Map~set(#MASM~floatRegs, $floatReg, $double);
}

var #MASM~stackPtr: #UInt64;
var #MASM~stack: #Map #UInt64 #RegData;

procedure #MASM~stackPush($data: #RegData)
  modifies #MASM~stackPtr, #MASM~stack;
{
    #MASM~stack := #Map~set(#MASM~stack, #MASM~stackPtr, $data);
    #MASM~stackPtr := #MASM~stackPtr + 8;
}

procedure #MASM~stackPop()
  returns (data: #RegData)
  modifies #MASM~stackPtr, #MASM~stack;
{
    data := #Map~get(#MASM~stack, #MASM~stackPtr);
    #MASM~stackPtr := #MASM~stackPtr - 8;
}

function #LiveRegisterSet~fromSetsUnchecked($regs: #Set #Reg, $floatRegs: #Set #FloatReg):
#LiveRegisterSet;

var #CacheIR~knownOperandIds: #Set #OperandId;

var #CacheIR~operandLocations: #Map #OperandId #OperandLocation;

procedure #CacheIR~defineReg($typedId: #TypedId)
    returns (reg: #Reg)
    modifies #CacheIR~operandLocations, #CacheIR~availableRegs;
{
    var $operandId'0: #OperandId;
    var $loc'0: #OperandLocation;
    var $loc'1: #OperandLocation;
    var $locKind'0: #OperandLocationKind;
    
    $operandId'0 := #TypedId^to#OperandId($typedId);
    $loc'0 := #Map~get(#CacheIR~operandLocations, $operandId'0);
    $locKind'0 := #OperandLocation~kind($loc'0);
    assert $locKind'0 == #OperandLocationKind^Variant~Uninitialized();
    
    call reg := #CacheIR~allocateReg();
    call $loc'1 := #OperandLocation~setPayloadReg(reg, #TypedId~type($typedId));
    #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $operandId'0, $loc'1);
}

procedure #CacheIR~defineValueReg($valueId: #ValueId)
    returns (reg: #ValueReg)
    modifies #CacheIR~operandLocations, #CacheIR~availableRegs;
{
    var $operandId'0: #OperandId;
    var $loc'0: #OperandLocation;
    var $loc'1: #OperandLocation;
    var $locKind'0: #OperandLocationKind;
    
    $operandId'0 := #ValueId^to#OperandId($valueId);
    $loc'0 := #Map~get(#CacheIR~operandLocations, $operandId'0);
    $locKind'0 := #OperandLocation~kind($loc'0);
    assert $locKind'0 == #OperandLocationKind^Variant~Uninitialized();
    
    call reg := #CacheIR~allocateValueReg();
    call $loc'1 := #OperandLocation~setValueReg(reg);
    #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $operandId'0, $loc'1);
}

procedure #CacheIR~getOperandLocation($operandId: #OperandId)
    returns (loc: #OperandLocation)
{
    loc := #Map~get(#CacheIR~operandLocations, $operandId);
}

procedure #CacheIR~setOperandLocation($operandId: #OperandId, $loc: #OperandLocation)
    modifies #CacheIR~operandLocations;
{
    #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $operandId, $loc);
}

procedure #initInputOperandLocation($typedId: #TypedId)
    modifies #CacheIR~operandLocations, #CacheIR~knownOperandIds;
{
    var $operandId'0: #OperandId;
    var tmp'0: #Bool;
    var $loc'0: #OperandLocation;

    $operandId'0 := #TypedId^to#OperandId($typedId);
    tmp'0 := #Set~contains(#CacheIR~knownOperandIds, $operandId'0);
    assume !tmp'0;
    #CacheIR~knownOperandIds := #Set~add(#CacheIR~knownOperandIds, $operandId'0);

    call $loc'0 := #OperandLocation~newUninitialized();
    #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $operandId'0, $loc'0);
}

procedure #initValueInputOperandLocation($valueId: #ValueId)
    modifies #CacheIR~operandLocations, #CacheIR~knownOperandIds;
{
    var $operandId'0: #OperandId;
    var tmp'0: #Bool;
    var $loc'0: #OperandLocation;

    $operandId'0 := #ValueId^to#OperandId($valueId);
    tmp'0 := #Set~contains(#CacheIR~knownOperandIds, $operandId'0);
    assume !tmp'0;
    #CacheIR~knownOperandIds := #Set~add(#CacheIR~knownOperandIds, $operandId'0);

    call $loc'0 := #OperandLocation~newUninitialized();
    #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $operandId'0, $loc'0);
}

procedure #initValueReg($valueReg: #ValueReg)
    modifies #MASM~regs;
{
    var $data'0: #RegData;
    var tmp'0: #Bool;

    call $data'0 := #MASM~getData($valueReg);
    call tmp'0 := #RegData~isValue($data'0);
    assume tmp'0;
}

// general purpose register that were live
// before the CacheIR stub.
var #CacheIR~liveRegs: #Set #Reg;

// general purpose registers that are available
// for the CacheIR stub to use.
var #CacheIR~availableRegs: #Set #Reg;

procedure #CacheIR~allocateValueReg()
  returns (ret: #ValueReg)
  modifies #CacheIR~availableRegs;
{
  call ret := #CacheIR~allocateReg();
}

procedure #CacheIR~releaseValueReg($valueReg: #ValueReg)
  modifies #CacheIR~availableRegs;
{
  call #CacheIR~releaseReg($valueReg);
}

procedure #CacheIR~allocateReg()
  returns (ret: #Reg)
  modifies #CacheIR~availableRegs;
{
  var $reg: #Reg;
  var tmp'0: #Bool;

  // ensure that we have enough registers by
  // asserting that atleast one allocatable
  // register is available
  assert (
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~Rax()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~Rbx()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~Rcx()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~Rdx()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~Rsi()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~Rdi()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~R8()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~R9()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~R10()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~R12()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~R13()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~R14()) ||
    #Set~contains(#CacheIR~availableRegs, #Reg^Variant~R15())
  );

  $reg := #CacheIR~allocateRegUnchecked(#CacheIR~availableRegs);

  // TODO: maybe this can be removed depending on 
  // how we populate availableRegs initially
  // rsp, rbp and r11 are not allocatable registers
  assume (
    $reg != #Reg^Variant~Rsp() &&
    $reg != #Reg^Variant~Rbp() &&
    $reg != #Reg^Variant~R11()
  );

  tmp'0 := #Set~contains(#CacheIR~availableRegs, $reg);
  assume tmp'0;

  #CacheIR~availableRegs := #Set~remove(#CacheIR~availableRegs, $reg);
  ret := $reg;
}

procedure #CacheIR~allocateKnownReg($reg: #Reg)
  modifies #CacheIR~availableRegs;
{
  // rsp, rbp and r11 are not allocatable registers
  assert (
    $reg != #Reg^Variant~Rsp() &&
    $reg != #Reg^Variant~Rbp() &&
    $reg != #Reg^Variant~R11()
  );

  // register should not have been previously allocated by the stub
  assert #Set~contains(#CacheIR~availableRegs, $reg);

  // make the register unavailable
  #CacheIR~availableRegs := #Set~remove(#CacheIR~availableRegs, $reg);
}

function #CacheIR~allocateRegUnchecked($availableRegs: #Set #Reg): #Reg;

procedure #CacheIR~releaseReg($reg: #Reg)
  modifies #CacheIR~availableRegs;
{
  // register should have been previously allocated by the stub
  assert !#Set~contains(#CacheIR~availableRegs, $reg);

  // make the register available again
  #CacheIR~availableRegs := #Set~add(#CacheIR~availableRegs, $reg);
}

// floating-point registers that are available
// for the CacheIR stub to use.
var #CacheIR~availableFloatRegs: #Set #FloatReg;

// floating-point registers that were live
// before the CacheIR stub.
var #CacheIR~liveFloatRegs: #Set #FloatReg;

procedure #CacheIR~allocateAvailableFloatReg($floatReg: #FloatReg)
  returns (ret: #FloatReg)
  modifies #CacheIR~availableFloatRegs;
{
  // xmm15 is not an allocatable register
  assert (
    $floatReg != #FloatReg^Variant~Xmm15()
  );

  call #CacheIR~allocateAvailableFloatRegUnchecked($floatReg: #FloatReg);
  ret := $floatReg;
}

procedure #CacheIR~allocateAvailableFloatRegUnchecked($floatReg: #FloatReg)
  modifies #CacheIR~availableFloatRegs;
{
  // register should not already be allocated
  assert #Set~contains(#CacheIR~availableFloatRegs, $floatReg);

  // make the register unavailable
  #CacheIR~availableFloatRegs := #Set~remove(#CacheIR~availableFloatRegs, $floatReg);
}

procedure #CacheIR~allocateFloatScratchReg()
  returns (ret: #FloatReg)
  modifies #CacheIR~availableFloatRegs;
{
  call #CacheIR~allocateAvailableFloatRegUnchecked(#FloatReg^Variant~Xmm15());
  ret := #FloatReg^Variant~Xmm15(); 
}

procedure #CacheIR~releaseFloatReg($floatReg: #FloatReg)
  modifies #CacheIR~availableFloatRegs;
{
  // register should have been allocated in the stub
  assert !#Set~contains(#CacheIR~availableFloatRegs, $floatReg);

  #CacheIR~availableFloatRegs := #Set~add(#CacheIR~availableFloatRegs, $floatReg);
}

procedure #initRegState() {
  assume (forall reg: #Reg :: !#Set~contains(#CacheIR~availableRegs, reg)); 
  assume (forall floatReg: #FloatReg :: !#Set~contains(#CacheIR~availableFloatRegs, floatReg)); 
}

procedure #addAvailableReg($reg: #Reg) 
  modifies #CacheIR~availableRegs;
{
  #CacheIR~availableRegs := #Set~add(#CacheIR~availableRegs, $reg);
}

procedure #addAvailableFloatReg($floatReg: #FloatReg) 
  modifies #CacheIR~availableFloatRegs;
{
  #CacheIR~availableFloatRegs := #Set~add(#CacheIR~availableFloatRegs, $floatReg);
}
