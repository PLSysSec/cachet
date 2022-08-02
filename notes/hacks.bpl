type #ValueReg = #Reg;

var #MASM~regs: #Map #Reg #Value;
    
procedure #MASM~getValue($valueReg: #ValueReg)
  returns (ret: #Value)
{
  ret := #Map~get(#MASM~regs, $valueReg);
}

procedure #MASM~setValue($valueReg: #ValueReg, $value: #Value)
  modifies #MASM~regs;
{
  #MASM~regs := #Map~set(#MASM~regs, $valueReg, $value);
}

procedure #MASM~getInt32($reg: #Reg)
  returns (ret: #Int32)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getValue($reg);
  call ret := #Value~toInt32(tmp'0);
}

procedure #MASM~setInt32($reg: #Reg, $int32: #Int32)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromInt32($int32);
  call #MASM~setValue($reg, tmp'0);
}

procedure #MASM~getBool($reg: #Reg)
  returns (ret: #Bool)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getValue($reg);
  call ret := #Value~toBool(tmp'0);
}

procedure #MASM~setBool($reg: #Reg, $bool: #Bool)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromBool($bool);
  call #MASM~setValue($reg, tmp'0);
}

procedure #MASM~getObject($reg: #Reg)
  returns (ret: #Object)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getValue($reg);
  call ret := #Value~toObject(tmp'0);
}

procedure #MASM~setObject($reg: #Reg, $object: #Object)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromObject($object);
  call #MASM~setValue($reg, tmp'0);
}

procedure #MASM~getString($reg: #Reg)
  returns (ret: #String)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getValue($reg);
  call ret := #Value~toString(tmp'0);
}

procedure #MASM~setString($reg: #Reg, $string: #String)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromString($string);
  call #MASM~setValue($reg, tmp'0);
}

procedure #MASM~getSymbol($reg: #Reg)
  returns (ret: #Symbol)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getValue($reg);
  call ret := #Value~toSymbol(tmp'0);
}

procedure #MASM~setSymbol($reg: #Reg, $symbol: #Symbol)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromSymbol($symbol);
  call #MASM~setValue($reg, tmp'0);
}

procedure #MASM~getBigInt($reg: #Reg)
  returns (ret: #BigInt)
{
  var tmp'0: #Value;
  call tmp'0 := #MASM~getValue($reg);
  call ret := #Value~toBigInt(tmp'0);
}

procedure #MASM~setBigInt($reg: #Reg, $bigInt: #BigInt)
  modifies #MASM~regs;
{
  var tmp'0: #Value;
  call tmp'0 := #Value~fromBigInt($bigInt);
  call #MASM~setValue($reg, tmp'0);
}

var #CacheIR~knownOperandIds: #Set #OperandId;

var #CacheIR~operandLocations: #Map #OperandId #OperandLocation;

procedure #CacheIR~defineReg($typedId: #TypedId)
    returns (reg: #Reg)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
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
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
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

procedure #CacheIR~defineObjectReg($id: #ObjectId)
  returns (ret: #Reg)
  modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromObjectId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineReg($typedId'v0);
  return;
}

procedure #CacheIR~defineInt32Reg($id: #Int32Id)
  returns (ret: #Reg)
  modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromInt32Id($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineReg($typedId'v0);
  return;
}

procedure #CacheIR~defineNumberReg($id: #NumberId)
  returns (ret: #ValueReg)
  modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $valueId'v0: #ValueId;
  
  $valueId'v0 := #NumberId^to#ValueId($id);
  call ret := #CacheIR~defineValueReg($valueId'v0);
  return;
}

procedure #CacheIR~defineBooleanReg($id: #BooleanId)
  returns (ret: #Reg)
  modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromBooleanId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineReg($typedId'v0);
  return;
}

procedure #CacheIR~defineStringReg($id: #StringId)
  returns (ret: #Reg)
  modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromStringId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineReg($typedId'v0);
  return;
}

procedure #CacheIR~defineSymbolReg($id: #SymbolId)
  returns (ret: #Reg)
  modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromSymbolId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineReg($typedId'v0);
  return;
}

procedure #CacheIR~defineBigIntReg($id: #BigIntId)
  returns (ret: #Reg)
  modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromBigIntId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~defineReg($typedId'v0);
  return;
}

procedure #CacheIR~useReg($typedId: #TypedId)
    returns (reg: #Reg)
{
    var $operandId'0: #OperandId;
    var $loc'0: #OperandLocation;
    var $locKind'0: #OperandLocationKind;
    
    $operandId'0 := #TypedId^to#OperandId($typedId);
    $loc'0 := #Map~get(#CacheIR~operandLocations, $operandId'0);
    $locKind'0 := #OperandLocation~kind($loc'0);

    if ($locKind'0 == #OperandLocationKind^Variant~PayloadReg()) {
        call reg := #OperandLocation~getPayloadReg($loc'0);
    } else {
        assert false;
    }
}

procedure #CacheIR~useValueReg($valueId: #ValueId)
    returns (reg: #ValueReg)
{
    var $operandId'0: #OperandId;
    var $loc'0: #OperandLocation;
    var $locKind'0: #OperandLocationKind;
    
    $operandId'0 := #ValueId^to#OperandId($valueId);
    $loc'0 := #Map~get(#CacheIR~operandLocations, $operandId'0);
    $locKind'0 := #OperandLocation~kind($loc'0);

    if ($locKind'0 == #OperandLocationKind^Variant~ValueReg()) {
        call reg := #OperandLocation~getValueReg($loc'0);
    } else if ($locKind'0 == #OperandLocationKind^Variant~Uninitialized()) { 
        assert false;
    } else if ($locKind'0 == #OperandLocationKind^Variant~PayloadReg()) {
        assert false;
    } else {
        assert false;
    }
}

procedure #CacheIR~useObjectReg($id: #ObjectId)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromObjectId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useReg($typedId'v0);
  return;
}

procedure #CacheIR~useInt32Reg($id: #Int32Id)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromInt32Id($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useReg($typedId'v0);
  return;
}

procedure #CacheIR~useNumberReg($id: #NumberId)
  returns (ret: #ValueReg)
{
  var $valueId'v0: #ValueId;
  
  $valueId'v0 := #NumberId^to#ValueId($id);
  call ret := #CacheIR~useValueReg($valueId'v0);
  return;
}

procedure #CacheIR~useBooleanReg($id: #BooleanId)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromBooleanId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useReg($typedId'v0);
  return;
}

procedure #CacheIR~useStringReg($id: #StringId)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromStringId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useReg($typedId'v0);
  return;
}

procedure #CacheIR~useSymbolReg($id: #SymbolId)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromSymbolId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useReg($typedId'v0);
  return;
}

procedure #CacheIR~useBigIntReg($id: #BigIntId)
  returns (ret: #Reg)
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromBigIntId($id);
  $typedId'v0 := out'0;
  call ret := #CacheIR~useReg($typedId'v0);
  return;
}

procedure #CacheIR~initInputLocation($typedId: #TypedId)
    modifies #CacheIR~operandLocations;
{
    var $operandId'0: #OperandId;
    var tmp'0: #Bool;
    var $loc'0: #OperandLocation;

    $operandId'0 := #TypedId^to#OperandId($typedId);
    call $loc'0 := #OperandLocation~newUninitialized();
    tmp'0 := #Set~contains(#CacheIR~knownOperandIds, $operandId'0);
    assume !tmp'0;
    #CacheIR~knownOperandIds := #Set~add(#CacheIR~knownOperandIds, $operandId'0);
    #CacheIR~operandLocations := #Map~set(#CacheIR~operandLocations, $operandId'0, $loc'0);
}

procedure #CacheIR~initValueInputLocation($valueId: #ValueId)
    modifies #CacheIR~operandLocations;
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

procedure #CacheIR~initInput($typedId: #TypedId)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var out'0: #Reg;
  
  call #CacheIR~initInputLocation($typedId);
  call out'0 := #CacheIR~defineReg($typedId);
  return;
}

procedure #CacheIR~initValueInput($valueId: #ValueId)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var out'0: #ValueReg;
  
  call #CacheIR~initValueInputLocation($valueId);
  call out'0 := #CacheIR~defineValueReg($valueId);
  return;
}

procedure #CacheIR~initObjectInput($id: #ObjectId)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromObjectId($id);
  $typedId'v0 := out'0;
  call #CacheIR~initInput($typedId'v0);
  return;
}

procedure #CacheIR~initInt32Input($id: #Int32Id)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromInt32Id($id);
  $typedId'v0 := out'0;
  call #CacheIR~initInput($typedId'v0);
  return;
}

procedure #CacheIR~initNumberInput($id: #NumberId)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $valueId'v0: #ValueId;
  
  $valueId'v0 := #NumberId^to#ValueId($id);
  call #CacheIR~initValueInput($valueId'v0);
  return;
}

procedure #CacheIR~initBooleanInput($id: #BooleanId)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromBooleanId($id);
  $typedId'v0 := out'0;
  call #CacheIR~initInput($typedId'v0);
  return;
}

procedure #CacheIR~initStringInput($id: #StringId)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromStringId($id);
  $typedId'v0 := out'0;
  call #CacheIR~initInput($typedId'v0);
  return;
}

procedure #CacheIR~initSymbolInput($id: #SymbolId)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromSymbolId($id);
  $typedId'v0 := out'0;
  call #CacheIR~initInput($typedId'v0);
  return;
}

procedure #CacheIR~initBigIntInput($id: #BigIntId)
    modifies #CacheIR~operandLocations, #CacheIR~allocatedRegs;
{
  var $typedId'v0: #TypedId;
  var out'0: #TypedId;
  
  call out'0 := #TypedId~fromBigIntId($id);
  $typedId'v0 := out'0;
  call #CacheIR~initInput($typedId'v0);
  return;
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

function #CacheIR~allocateRegUnchecked($allocatedRegs: #Set #Reg): #Reg;

procedure #CacheIR~releaseReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  #CacheIR~allocatedRegs := #Set~remove(#CacheIR~allocatedRegs, $reg);
}

