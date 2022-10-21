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
  var $reg: #Reg;
  var tmp'0: #Bool;
  $reg := #CacheIR~allocateRegUnchecked(#CacheIR~allocatedRegs);
  tmp'0 := #Set~contains(#CacheIR~allocatedRegs, $reg);
  assume !tmp'0;
  #CacheIR~allocatedRegs := #Set~add(#CacheIR~allocatedRegs, $reg);
  ret := $reg;
}

procedure #CacheIR~allocateKnownReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  var tmp'0: #Bool;
  tmp'0 := #Set~contains(#CacheIR~allocatedRegs, $reg);
  assume !tmp'0;
  #CacheIR~allocatedRegs := #Set~add(#CacheIR~allocatedRegs, $reg);
}

function #CacheIR~allocateRegUnchecked($allocatedRegs: #Set #Reg): #Reg;

procedure #CacheIR~releaseReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  #CacheIR~allocatedRegs := #Set~remove(#CacheIR~allocatedRegs, $reg);
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

