type #ValueReg = #Reg;

procedure #ValueReg~scratchReg($valueReg: #ValueReg)
  returns (reg: #Reg)
{
  reg := $valueReg;
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

var #MASM~floatRegs: #Map #PhyFloatReg #RegData;

procedure #MASM~getFloatData($phyReg: #PhyFloatReg)
  returns (ret: #RegData)
{
  ret := #Map~get(#MASM~floatRegs, $phyReg);
}

procedure #MASM~setFloatData($phyReg: #PhyFloatReg, $data: #RegData)
  modifies #MASM~floatRegs;
{
    #MASM~floatRegs := #Map~set(#MASM~floatRegs, $phyReg, $data);
}

procedure #MASM~getDouble($floatReg: #FloatReg)
  returns (ret: #Double)
{
    var tmp'0: #RegData;
    var tmp'1: #Value;

    assert #FloatReg^field~type($floatReg) == #FloatContentType^Variant~Double();
    call tmp'0 := #MASM~getFloatData(#FloatReg^field~reg($floatReg));
    call tmp'1 := #RegData~toUnboxedValue(tmp'0);
    call ret := #Value~toDouble(tmp'1);
}

procedure #MASM~setDouble($floatReg: #FloatReg, $double: #Double)
{
    var tmp'0: #Value;
    var tmp'1: #RegData;

    assert #FloatReg^field~type($floatReg) == #FloatContentType^Variant~Double();
    call tmp'0 := #Value~fromDouble($double);
    call tmp'1 := #RegData~fromUnboxedValue(tmp'0);
    call #MASM~setFloatData(#FloatReg^field~reg($floatReg), tmp'1);
}

var #CacheIR~knownOperandIds: #Set #UInt16;

var #CacheIR~operandLocations: #Map #UInt16 #OperandLocation;

procedure #CacheIR~defineTypedId($typedId: #TypedId)
    returns (ret: #Reg)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
    var $id'0: #UInt16;
    var $loc'0: #OperandLocation;
    var $loc'1: #OperandLocation;
    var $locKind'0: #OperandLocationKind;
    
    $id'0 := #OperandId~id(#TypedId^to#OperandId($typedId));
    $loc'0 := #Map~get(#CacheIR~operandLocations, $id'0);
    $locKind'0 := #OperandLocation~kind($loc'0);
    assert $locKind'0 == #OperandLocationKind^Variant~Uninitialized();
    
    call ret := #CacheIR~allocateReg();
    call $loc'1 := #OperandLocation~setPayloadReg(ret, #TypedId~type($typedId));
    #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $id'0, $loc'1);
}

procedure #CacheIR~defineValueId($valueId: #ValueId)
    returns (ret: #ValueReg)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
    var $id'0: #UInt16;
    var $loc'0: #OperandLocation;
    var $loc'1: #OperandLocation;
    var $locKind'0: #OperandLocationKind;
    
    $id'0 := #OperandId~id(#ValueId^to#OperandId($valueId));
    $loc'0 := #Map~get(#CacheIR~operandLocations, $id'0);
    $locKind'0 := #OperandLocation~kind($loc'0);
    assert $locKind'0 == #OperandLocationKind^Variant~Uninitialized();
    
    call ret := #CacheIR~allocateValueReg();
    call $loc'1 := #OperandLocation~setValueReg(ret);
    #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $id'0, $loc'1);
}

procedure #CacheIR~getOperandLocation($operandId: #OperandId)
    returns (loc: #OperandLocation)
{
    loc := #Map~get(#CacheIR~operandLocations, #OperandId~id($operandId));
}

procedure #CacheIR~setOperandLocation($operandId: #OperandId, $loc: #OperandLocation)
    modifies #CacheIR~operandLocations;
{
    #CacheIR~operandLocations :=
        #Map~set(#CacheIR~operandLocations, #OperandId~id($operandId), $loc);
}

var #CacheIR~allocatedRegs: #Set #Reg;

procedure #CacheIR~allocateValueReg()
  returns (ret: #ValueReg)
  modifies #CacheIR~allocatedRegs;
{
  call ret := #CacheIR~allocateReg();
}

procedure #CacheIR~releaseValueReg($valueReg: #ValueReg)
  modifies #CacheIR~allocatedRegs;
{
  call #CacheIR~releaseReg($valueReg);
}

procedure #CacheIR~allocateReg()
  returns (ret: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  var tmp'0: #Bool;

  // ensure that we have enough registers by
  // asserting that there is atleast one allocatable
  // register that is not already allocated
  assert (
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rax()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rbx()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rcx()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rdx()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rsi()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rdi()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R8()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R9()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R10()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R12()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R13()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R14()) ||
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R15())
  );

  ret := #CacheIR~allocateRegUnchecked(#CacheIR~allocatedRegs);

  // rsp, rbp and r11 are not allocatable registers
  // NOTE: rbp is allocatable in the latest versions of Firefox
  // but is not in the cachet fork
  assume (
    ret != #Reg^Variant~Rsp() &&
    ret != #Reg^Variant~Rbp() &&
    ret != #Reg^Variant~R11()
  );

  tmp'0 := #Set~contains(#CacheIR~allocatedRegs, ret);
  assume !tmp'0;

  #CacheIR~allocatedRegs := #Set~add(#CacheIR~allocatedRegs, ret);
}

procedure #CacheIR~allocateKnownReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  var tmp'0: #Bool;
  
  // register should not already be allocated
  tmp'0 := #Set~contains(#CacheIR~allocatedRegs, $reg);
  assert !tmp'0;

  #CacheIR~allocatedRegs := #Set~add(#CacheIR~allocatedRegs, $reg);
}

function #CacheIR~allocateRegUnchecked($allocatedRegs: #Set #Reg): #Reg;

procedure #CacheIR~releaseReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  var tmp'0: #Bool;

  // register should already be allocated
  tmp'0 := #Set~contains(#CacheIR~allocatedRegs, $reg);
  assert tmp'0;

  #CacheIR~allocatedRegs := #Set~remove(#CacheIR~allocatedRegs, $reg);
}

var #CacheIR~allocatedFloatRegs: #Set #PhyFloatReg;

procedure #CacheIR~allocateAvailableFloatReg($floatReg: #FloatReg)
  returns (ret: #FloatReg)
  modifies #CacheIR~allocatedFloatRegs;
{
  // xmm15 is not an allocatable register
  assert (
    #FloatReg^field~reg($floatReg) != #PhyFloatReg^Variant~Xmm15()
  );

  call #CacheIR~allocateAvailableFloatRegUnchecked($floatReg: #FloatReg);
  ret := $floatReg;
}

procedure #CacheIR~allocateAvailableFloatRegUnchecked($floatReg: #FloatReg)
  modifies #CacheIR~allocatedFloatRegs;
{
  // register should not already be allocated
  assert !#Set~contains(#CacheIR~allocatedFloatRegs, #FloatReg^field~reg($floatReg));

  #CacheIR~allocatedFloatRegs := #Set~add(#CacheIR~allocatedFloatRegs, #FloatReg^field~reg($floatReg));
}

procedure #CacheIR~releaseFloatReg($floatReg: #FloatReg)
  modifies #CacheIR~allocatedFloatRegs;
{
  // register should be allocated
  assert #Set~contains(#CacheIR~allocatedFloatRegs, #FloatReg^field~reg($floatReg));

  #CacheIR~allocatedFloatRegs := #Set~remove(#CacheIR~allocatedFloatRegs, #FloatReg^field~reg($floatReg));
}

procedure #initRegState()
{
  // initially all registers are unallocated
  assume (
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rax()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rbx()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rcx()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rdx()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rsp()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rbp()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rsi()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rdi()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R8()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R9()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R10()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R11()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R12()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R13()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R14()) &&
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R15())
  );

  // initially all registers are unallocated
  assume (
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm0()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm1()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm2()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm3()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm4()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm5()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm6()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm7()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm8()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm9()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm10()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm11()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm12()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm13()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm14()) &&
    !#Set~contains(#CacheIR~allocatedFloatRegs, #PhyFloatReg^Variant~Xmm15())
  );
}

procedure #initOperandId($operandId: #OperandId)
  modifies #CacheIR~operandLocations, #CacheIR~knownOperandIds;
{
  var $id'0: #UInt16;
  var tmp'0: #Bool;
  var $loc'0: #OperandLocation;

  $id'0 := #OperandId~id($operandId);
  tmp'0 := #Set~contains(#CacheIR~knownOperandIds, $id'0);
  assume !tmp'0;
  #CacheIR~knownOperandIds := #Set~add(#CacheIR~knownOperandIds, $id'0);

  call $loc'0 := #OperandLocation~newUninitialized();
  #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $id'0, $loc'0);
}

procedure #initInputValueId($valueId: #ValueId)
  modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $valueReg'0: #ValueReg;
  var $data'1: #RegData;
  var tmp'2: #Bool;
  call $valueReg'0 := #CacheIR~defineValueId($valueId);
  call $data'1 := #MASM~getData($valueReg'0);
  call tmp'2 := #RegData~isValue($data'1);
  assume tmp'2;
}

procedure #initValueOutput()
{
  var tmp'0: #Bool;
  var tmp'1: #ValueReg;

  call tmp'0 := #TypedOrValueReg~hasValue(#CacheIR~outputReg);
  assume tmp'0;

  call tmp'1 := #TypedOrValueReg~toValueReg(#CacheIR~outputReg);
  assume !#Set~contains(#CacheIR~allocatedRegs, tmp'1);
}

procedure #initTypedOutput($type: #MIRType)
{
  var tmp'0: #MIRType;
  var $anyReg'1: #AnyReg;
  var tmp'2: #Bool;
  var tmp'3: #Reg;

  assert $type != #MIRType^Variant~Value();
  assume #TypedOrValueReg~type(#CacheIR~outputReg) == $type;

  call $anyReg'1 := #TypedOrValueReg~toTypedReg(#CacheIR~outputReg);
  assume !#AnyReg~isFloat($anyReg'1);

  call tmp'3 := #AnyReg~toReg($anyReg'1);
  assume !#Set~contains(#CacheIR~allocatedRegs, tmp'3);
}
