type #ValueReg = #Reg;

procedure #ValueReg~scratchReg($valueReg: #ValueReg)
  returns (reg: #Reg)
{
  reg := $valueReg;
}

procedure #ValueReg~payloadOrValueReg($valueReg: #ValueReg)
  returns (reg: #Reg)
{
  reg := $valueReg;
}

var #MASM~regs: #Map #Reg #Data;

procedure #MASM~getData($reg: #Reg)
  returns (ret: #Data)
{
  ret := #Map~get(#MASM~regs, $reg);
}

procedure #MASM~setData($reg: #Reg, $data: #Data)
  modifies #MASM~regs;
{
  #MASM~regs := #Map~set(#MASM~regs, $reg, $data);
}

procedure #MASM~getStackIndex($reg: #Reg)
  returns (ret: #UInt64)
{
  var tmp'0: #Data;
  call tmp'0 := #MASM~getData($reg);
  call ret := #Data~toStackIndex(tmp'0);
}

procedure #MASM~setStackIndex($reg: #Reg, $index: #UInt64)
  modifies #MASM~regs;
{
  var tmp'0: #Data;
  call tmp'0 := #Data~fromStackIndex($index);
  call #MASM~setData($reg, tmp'0);
}
    
procedure #MASM~getValue($valueReg: #ValueReg)
  returns (ret: #Value)
{
  var tmp'0: #Data;
  call tmp'0 := #MASM~getData($valueReg);
  call ret := #Data~toValue(tmp'0);
}

procedure #MASM~setValue($valueReg: #ValueReg, $value: #Value)
  modifies #MASM~regs;
{
  var tmp'0: #Data;
  call tmp'0 := #Data~fromValue($value);
  call #MASM~setData($valueReg, tmp'0);
}

procedure #MASM~getUnboxedValue($reg: #Reg)
  returns (ret: #Value)
{
  var tmp'0: #Data;
  call tmp'0 := #MASM~getData($reg);
  call ret := #Data~toUnboxedValue(tmp'0);
}

procedure #MASM~setUnboxedValue($reg: #Reg, $value: #Value)
  modifies #MASM~regs;
{
  var tmp'0: #Data;
  call tmp'0 := #Data~fromUnboxedValue($value);
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

var #MASM~floatRegs: #Map #PhyFloatReg #FloatData;

procedure #MASM~getFloatData($phyReg: #PhyFloatReg)
  returns (ret: #FloatData)
{
  ret := #Map~get(#MASM~floatRegs, $phyReg);
}

procedure #MASM~setFloatData($phyReg: #PhyFloatReg, $data: #FloatData)
  modifies #MASM~floatRegs;
{
    #MASM~floatRegs := #Map~set(#MASM~floatRegs, $phyReg, $data);
}

procedure #MASM~getDouble($floatReg: #FloatReg)
  returns (ret: #Double)
{
    var tmp'0: #FloatData;

    assert #FloatReg^field~type($floatReg) == #FloatContentType^Variant~Double();
    call tmp'0 := #MASM~getFloatData(#FloatReg^field~reg($floatReg));
    call ret := #FloatData~toDouble(tmp'0);
}

procedure #MASM~setDouble($floatReg: #FloatReg, $double: #Double)
{
    var tmp'0: #FloatData;

    assert #FloatReg^field~type($floatReg) == #FloatContentType^Variant~Double();
    call tmp'0 := #FloatData~fromDouble($double);
    call #MASM~setFloatData(#FloatReg^field~reg($floatReg), tmp'0);
}

var #MASM~stack: #Map #UInt64 #Data;

procedure #MASM~stackPush($data: #Data)
    modifies #MASM~regs, #MASM~stack;
{
    var $stackPtr: #UInt64;

    call $stackPtr := #MASM~getStackIndex(#Reg^Variant~Rsp());
    assert #UInt64^gte($stackPtr, 8bv64); 
    $stackPtr := #UInt64^sub($stackPtr, 8bv64);
    call #MASM~setStackIndex(#Reg^Variant~Rsp(), $stackPtr);
    #MASM~stack := #Map~set(#MASM~stack, $stackPtr, $data);
}

procedure #MASM~stackPop()
    returns (data: #Data)
    modifies #MASM~regs, #MASM~stack;
{
    var $newData: #Data;
    var $stackPtr: #UInt64;

    call $stackPtr := #MASM~getStackIndex(#Reg^Variant~Rsp());
    data := #Map~get(#MASM~stack, $stackPtr);
    havoc $newData;
    #MASM~stack := #Map~set(#MASM~stack, $stackPtr, $newData);
    $stackPtr := #UInt64^add($stackPtr, 8bv64);
    call #MASM~setStackIndex(#Reg^Variant~Rsp(), $stackPtr);
}

procedure #LiveRegisterSet~addReg($set: #LiveRegisterSet, $reg: #Reg)
    returns (newSet: #LiveRegisterSet)
{
    var $regSet: #Set #Reg;
    var $newRegSet: #Set #Reg;
    var $floatRegSet: #Set #FloatReg;

    $newRegSet := #Set~add($regSet, $reg);
    $floatRegSet := #LiveRegisterSet~getFloatRegSet($set);
    call newSet := #LiveRegisterSet~new($newRegSet, $floatRegSet);
}

procedure #LiveRegisterSet~takeReg($set: #LiveRegisterSet, $reg: #Reg)
    returns (newSet: #LiveRegisterSet)
{
    var $regSet: #Set #Reg;
    var $newRegSet: #Set #Reg;
    var $floatRegSet: #Set #FloatReg;

    $newRegSet := #Set~remove($regSet, $reg);
    $floatRegSet := #LiveRegisterSet~getFloatRegSet($set);
    call newSet := #LiveRegisterSet~new($newRegSet, $floatRegSet);
}

function #LiveRegisterSet~containsReg($set: #LiveRegisterSet, $reg: #Reg): #Bool
{
    #Set~contains(#LiveRegisterSet~getRegSet($set), $reg)    
}

function #LiveRegisterSet~containsFloatReg($set: #LiveRegisterSet, $floatReg: #FloatReg): #Bool
{
    #Set~contains(#LiveRegisterSet~getFloatRegSet($set), $floatReg)    
}

procedure #LiveRegisterSet~newVolatile()
    returns (set: #LiveRegisterSet)
{
    set := #LiveRegisterSet~newVolatileUnchecked(#LiveRegisterSet~getFloatRegSet(#CacheIR~liveRegs)); 
    assume #LiveRegisterSet~getFloatRegSet(set) == #LiveRegisterSet~getFloatRegSet(#CacheIR~liveRegs);
    assume #Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~Rax());
    assume !#Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~Rbx());
    assume #Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~Rcx());
    assume #Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~Rdx());
    assume !#Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~Rsp());
    assume !#Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~Rbp());
    assume #Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~Rsi());
    assume #Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~Rdi());
    assume #Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~R8());
    assume #Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~R9());
    assume #Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~R10());
    assume !#Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~R11());
    assume !#Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~R12());
    assume !#Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~R13());
    assume !#Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~R14());
    assume !#Set~contains(#LiveRegisterSet~getRegSet(set), #Reg^Variant~R15());
}

function #LiveRegisterSet~newVolatileUnchecked($liveFloatRegs: #Set #FloatReg): #LiveRegisterSet;

procedure #LiveRegisterSet~newEmpty()
    returns (set: #LiveRegisterSet)
{
    set := #LiveRegisterSet~newEmptyUnchecked();
    assume (forall reg: #Reg :: !#Set~contains(#LiveRegisterSet~getRegSet(set), reg)); 
    assume (forall floatReg: #FloatReg :: !#Set~contains(#LiveRegisterSet~getFloatRegSet(set), floatReg));
}

function #LiveRegisterSet~newEmptyUnchecked(): #LiveRegisterSet;

procedure #LiveRegisterSet~new($regSet: #Set #Reg, $floatRegSet: #Set #FloatReg)
    returns (set: #LiveRegisterSet)
{
    set := #LiveRegisterSet~newUnchecked($regSet, $floatRegSet);
    assume #LiveRegisterSet~getRegSet(set) == $regSet;
    assume #LiveRegisterSet~getFloatRegSet(set) == $floatRegSet;
}

function #LiveRegisterSet~newUnchecked($regSet: #Set #Reg, $floatRegSet: #Set #FloatReg): #LiveRegisterSet;

function #LiveRegisterSet~getRegSet($set: #LiveRegisterSet): #Set #Reg;
function #LiveRegisterSet~getFloatRegSet($set: #LiveRegisterSet): #Set #FloatReg;

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
    var $data'0: #Data;
    var tmp'0: #Bool;

    call $data'0 := #MASM~getData($valueReg);
    call tmp'0 := #Data~isValue($data'0);
    assume tmp'0;
}

var #CacheIR~allocatedRegs: #Set #Reg;

procedure #initAllocatedRegs()
  modifies #CacheIR~allocatedRegs;
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
}

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

  $reg := #CacheIR~allocateRegUnchecked(#CacheIR~allocatedRegs);

  // rsp, rbp and r11 are not allocatable registers
  // TODO: rbp is allocatable in newer versions of firefox
  assume (
    $reg != #Reg^Variant~Rsp() &&
    $reg != #Reg^Variant~Rbp() &&
    $reg != #Reg^Variant~R11()
  );

  tmp'0 := #Set~contains(#CacheIR~allocatedRegs, $reg);
  assume !tmp'0;

  #CacheIR~allocatedRegs := #Set~add(#CacheIR~allocatedRegs, $reg);
  ret := $reg;
}

procedure #CacheIR~allocateKnownReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  // rsp, rbp and r11 are not allocatable registers
  // TODO: rbp is allocatable in newer versions of firefox
  assert (
    $reg != #Reg^Variant~Rsp() &&
    $reg != #Reg^Variant~Rbp() &&
    $reg != #Reg^Variant~R11()
  );

  // register should not already be allocated
  assert !#Set~contains(#CacheIR~allocatedRegs, $reg);

  #CacheIR~allocatedRegs := #Set~add(#CacheIR~allocatedRegs, $reg);
}

function #CacheIR~allocateRegUnchecked($allocatedRegs: #Set #Reg): #Reg;

procedure #CacheIR~releaseReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  // register should be allocated
  assert #Set~contains(#CacheIR~allocatedRegs, $reg);

  #CacheIR~allocatedRegs := #Set~remove(#CacheIR~allocatedRegs, $reg);
}

const #CacheIR~liveRegs: #LiveRegisterSet;

var #CacheIR~allocatedFloatRegs: #Set #PhyFloatReg;

procedure #initAllocatedFloatRegs()
  modifies #CacheIR~allocatedFloatRegs;
{
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

procedure #CacheIR~allocateFloatScratchReg()
  returns (ret: #FloatReg)
  modifies #CacheIR~allocatedFloatRegs;
{
  call ret := #FloatReg~new(#PhyFloatReg^Variant~Xmm15(), #FloatContentType^Variant~Double()); 
  call #CacheIR~allocateAvailableFloatRegUnchecked(ret);
}

procedure #CacheIR~releaseFloatReg($floatReg: #FloatReg)
  modifies #CacheIR~allocatedFloatRegs;
{
  // register should be allocated
  assert #Set~contains(#CacheIR~allocatedFloatRegs, #FloatReg^field~reg($floatReg));

  #CacheIR~allocatedFloatRegs := #Set~remove(#CacheIR~allocatedFloatRegs, #FloatReg^field~reg($floatReg));
}

procedure #initRegAllocator()
  modifies #CacheIR~allocatedRegs, #CacheIR~allocatedFloatRegs;
{
    call #initAllocatedRegs();
    call #initAllocatedFloatRegs();
    call #MASM~setStackIndex(#Reg^Variant~Rsp(), 512bv64);
}
