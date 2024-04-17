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

// GeneralRegSet

function #GeneralRegSet~rawSet($set: #GeneralRegSet): #Set #Reg;
function #GeneralRegSet~newUnchecked($rawSet: #Set #Reg): #GeneralRegSet;

procedure #GeneralRegSet~new($rawSet: #Set #Reg)
    returns (ret: #GeneralRegSet)
{
    var tmp'0: #GeneralRegSet;

    tmp'0 := #GeneralRegSet~newUnchecked($rawSet);
    assume #GeneralRegSet~rawSet(tmp'0) == $rawSet;
    ret := tmp'0;
}

procedure #GeneralRegSet~empty()
    returns (ret: #GeneralRegSet)
{
    var tmp'0: #Set #Reg;

    tmp'0 := #Set~empty();
    call ret := #GeneralRegSet~new(tmp'0);
}

procedure #GeneralRegSet~volatile()
    returns (ret: #GeneralRegSet)
{
    var tmp'0: #Set #Reg;

    tmp'0 := #Set~empty();
    tmp'0 := #Set~add(tmp'0, #Reg^Variant~Rax());
    tmp'0 := #Set~add(tmp'0, #Reg^Variant~Rcx());
    tmp'0 := #Set~add(tmp'0, #Reg^Variant~Rdx());
    tmp'0 := #Set~add(tmp'0, #Reg^Variant~Rsi());
    tmp'0 := #Set~add(tmp'0, #Reg^Variant~Rdi());
    tmp'0 := #Set~add(tmp'0, #Reg^Variant~R8());
    tmp'0 := #Set~add(tmp'0, #Reg^Variant~R9());
    tmp'0 := #Set~add(tmp'0, #Reg^Variant~R10());

    call ret := #GeneralRegSet~new(tmp'0);
}

procedure #GeneralRegSet~intersect($lhs: #GeneralRegSet, $rhs: #GeneralRegSet)
    returns (ret: #GeneralRegSet)
{
    var tmp'0: #Set #Reg;
    var tmp'1: #Set #Reg;

    tmp'0 := #GeneralRegSet~rawSet($lhs);
    tmp'1 := #GeneralRegSet~rawSet($rhs);

    call ret := #GeneralRegSet~new(#Set~intersect(tmp'0, tmp'1));
}

procedure #GeneralRegSet~difference($lhs: #GeneralRegSet, $rhs: #GeneralRegSet)
    returns (ret: #GeneralRegSet)
{
    var tmp'0: #Set #Reg;
    var tmp'1: #Set #Reg;

    tmp'0 := #GeneralRegSet~rawSet($lhs);
    tmp'1 := #GeneralRegSet~rawSet($rhs);

    call ret := #GeneralRegSet~new(#Set~difference(tmp'0, tmp'1));
}

function {:inline} #GeneralRegSet~contains($set: #GeneralRegSet, $reg: #Reg): #Bool {
    #Set~contains(#GeneralRegSet~rawSet($set), $reg)
}

procedure #GeneralRegSet~add($set: #GeneralRegSet, $reg: #Reg)
    returns (newSet: #GeneralRegSet)
{
    var $rawSet: #Set #Reg;

    $rawSet := #Set~add(#GeneralRegSet~rawSet($set), $reg);
    call newSet := #GeneralRegSet~new($rawSet);
}

procedure #GeneralRegSet~take($set: #GeneralRegSet, $reg: #Reg)
    returns (newSet: #GeneralRegSet)
{
    var $rawSet: #Set #Reg;

    $rawSet := #Set~remove(#GeneralRegSet~rawSet($set), $reg);
    call newSet := #GeneralRegSet~new($rawSet);
}

// FloatRegSet

function #FloatRegSet~rawSet($set: #FloatRegSet): #Set #FloatReg;
function #FloatRegSet~newUnchecked($rawSet: #Set #FloatReg): #FloatRegSet;

procedure #FloatRegSet~new($rawSet: #Set #FloatReg)
    returns (ret: #FloatRegSet)
{
    var tmp'0: #FloatRegSet;

    tmp'0 := #FloatRegSet~newUnchecked($rawSet);
    assume #FloatRegSet~rawSet(tmp'0) == $rawSet;
    ret := tmp'0;
}

procedure #FloatRegSet~empty()
    returns (ret: #FloatRegSet)
{
    var tmp'0: #Set #FloatReg;

    tmp'0 := #Set~empty();
    call ret := #FloatRegSet~new(tmp'0);
}

procedure #FloatRegSet~volatile()
    returns (ret: #FloatRegSet)
{
    var simdReg: #FloatReg;
    var doubleReg: #FloatReg;
    var singleReg: #FloatReg;
    var tmp'0: #Set #FloatReg;

    call simdReg := #FloatReg~new(#PhyFloatReg^Variant~Xmm15(), #FloatContentType^Variant~Simd128());
    call doubleReg := #FloatReg~new(#PhyFloatReg^Variant~Xmm15(), #FloatContentType^Variant~Double());
    call singleReg := #FloatReg~new(#PhyFloatReg^Variant~Xmm15(), #FloatContentType^Variant~Single());

    tmp'0 := #Set~all();
    tmp'0 := #Set~remove(tmp'0, simdReg);
    tmp'0 := #Set~remove(tmp'0, doubleReg);
    tmp'0 := #Set~remove(tmp'0, singleReg);

    call ret := #FloatRegSet~new(tmp'0);
}

procedure #FloatRegSet~intersect($lhs: #FloatRegSet, $rhs: #FloatRegSet)
    returns (ret: #FloatRegSet)
{
    var tmp'0: #Set #FloatReg;
    var tmp'1: #Set #FloatReg;

    tmp'0 := #FloatRegSet~rawSet($lhs);
    tmp'1 := #FloatRegSet~rawSet($rhs);

    call ret := #FloatRegSet~new(#Set~intersect(tmp'0, tmp'1));
}

procedure #FloatRegSet~difference($lhs: #FloatRegSet, $rhs: #FloatRegSet)
    returns (ret: #FloatRegSet)
{
    var tmp'0: #Set #FloatReg;
    var tmp'1: #Set #FloatReg;

    tmp'0 := #FloatRegSet~rawSet($lhs);
    tmp'1 := #FloatRegSet~rawSet($rhs);

    call ret := #FloatRegSet~new(#Set~difference(tmp'0, tmp'1));
}

function {:inline} #FloatRegSet~contains($set: #FloatRegSet, $reg: #FloatReg): #Bool {
    #Set~contains(#FloatRegSet~rawSet($set), $reg)
}

procedure #FloatRegSet~add($set: #FloatRegSet, $reg: #FloatReg)
    returns (newSet: #FloatRegSet)
{
    var $rawSet: #Set #FloatReg;

    $rawSet := #Set~add(#FloatRegSet~rawSet($set), $reg);
    call newSet := #FloatRegSet~new($rawSet);
}

procedure #FloatRegSet~take($set: #FloatRegSet, $reg: #FloatReg)
    returns (newSet: #FloatRegSet)
{
    var $rawSet: #Set #FloatReg;

    $rawSet := #Set~remove(#FloatRegSet~rawSet($set), $reg);
    call newSet := #FloatRegSet~new($rawSet);
}

var #MASM~pushedLiveGeneralRegs: #Map #Reg #RegData;
var #MASM~pushedLiveFloatRegs: #Map #PhyFloatReg #FloatData;

//procedure #MASM~stackPushLiveGeneralReg($reg: #Reg)
//{
//    var $data: #RegData;
//    call $data := #MASM~getData($reg);
//    #MASM~pushedLiveGeneralRegs := #Map~set(#MASM~pushedLiveGeneralRegs, $reg, $data);
//}
function #MASM~stackPushLiveGeneralRegs~raw(
  $pushedLiveGeneralRegs: #Map #Reg #RegData,
  $regs: #Map #Reg #RegData,
  #gprs: #GeneralRegSet
): #Map #Reg #RegData;
procedure #MASM~stackPushLiveGeneralRegs($gprs: #GeneralRegSet)
{
  var $newPushedLiveGeneralRegs: #Map #Reg #RegData;

  $newPushedLiveGeneralRegs := #MASM~stackPushLiveGeneralRegs~raw(
    #MASM~pushedLiveGeneralRegs, #MASM~regs, $gprs
  );

  assume (forall r:#Reg ::
    #Set~contains(#GeneralRegSet~rawSet($gprs), r) ==>
      (#Map~get($newPushedLiveGeneralRegs, r) == #Map~get(#MASM~regs, r)));
  assume (forall r:#Reg ::
    !#Set~contains(#GeneralRegSet~rawSet($gprs), r) ==>
      (#Map~get($newPushedLiveGeneralRegs, r) == #Map~get(#MASM~pushedLiveGeneralRegs, r)));

//  assume (forall r:#Reg :: #Map~get($newPushedLiveGeneralRegs, r) ==
//    (if #Set~contains(#GeneralRegSet~rawSet($gprs), r)
//      then #Map~get(#MASM~regs, r)
//      else #Map~get(#MASM~pushedLiveGeneralRegs, r)));

//  assume (forall r:#Reg :: #Map~get($newPushedLiveGeneralRegs, r) == #Map~get(
//    (if #Set~contains(#GeneralRegSet~rawSet($gprs), r)
//      then #MASM~regs
//      else #MASM~pushedLiveGeneralRegs),
//    r
//  ));

  #MASM~pushedLiveGeneralRegs := $newPushedLiveGeneralRegs;
}

//procedure #MASM~stackPopLiveGeneralReg($reg: #Reg)
//{
//    var $data: #RegData;
//    $data := #Map~get(#MASM~pushedLiveGeneralRegs, $reg);
//    call #MASM~setData($reg, $data);
//}
function #MASM~stackPopLiveGeneralRegs~raw(
  $regs: #Map #Reg #RegData,
  $pushedLiveGeneralRegs: #Map #Reg #RegData,
  #gprs: #GeneralRegSet,
  #gprsIgnore: #GeneralRegSet
): #Map #Reg #RegData;
procedure #MASM~stackPopLiveGeneralRegs($gprs: #GeneralRegSet, $gprsIgnore: #GeneralRegSet)
{
  var $newRegs: #Map #Reg #RegData;

  $newRegs := #MASM~stackPopLiveGeneralRegs~raw(
    #MASM~regs, #MASM~pushedLiveGeneralRegs, $gprs, $gprsIgnore
  );

  assume (forall r:#Reg ::
    (#Set~contains(#GeneralRegSet~rawSet($gprs), r) &&
        !#Set~contains(#GeneralRegSet~rawSet($gprsIgnore), r)) ==>
      (#Map~get($newRegs, r) == #Map~get(#MASM~pushedLiveGeneralRegs, r)));
  assume (forall r:#Reg ::
    !(#Set~contains(#GeneralRegSet~rawSet($gprs), r) &&
        !#Set~contains(#GeneralRegSet~rawSet($gprsIgnore), r)) ==>
      (#Map~get($newRegs, r) == #Map~get(#MASM~regs, r)));

//  assume (forall r:#Reg :: #Map~get($newRegs, r) ==
//    (if #Set~contains(#GeneralRegSet~rawSet($gprs), r)
//      then #Map~get(#MASM~pushedLiveGeneralRegs, r)
//      else #Map~get(#MASM~regs, r)));

//  assume (forall r:#Reg :: #Map~get($newRegs, r) == #Map~get(
//    (if #Set~contains(#GeneralRegSet~rawSet($gprs), r)
//      then #MASM~pushedLiveGeneralRegs
//      else #MASM~regs),
//    r
//  ));

  #MASM~regs := $newRegs;
}

//procedure #MASM~stackPushLiveFloatReg($floatReg: #FloatReg)
//{
//    var $data: #FloatData;
//    call $data := #MASM~getFloatData(#FloatReg^field~reg($floatReg));
//    assert #FloatReg^field~type($floatReg) == #FloatData~contentType($data);
//    #MASM~pushedLiveFloatRegs := #Map~set(#MASM~pushedLiveFloatRegs, #FloatReg^field~reg($floatReg), $data);
//}
function #MASM~stackPushLiveFloatRegs~raw(
  $pushedLiveFloatRegs: #Map #PhyFloatReg #FloatData,
  $floatRegs: #Map #PhyFloatReg #FloatData,
  #fpus: #FloatRegSet
): #Map #PhyFloatReg #FloatData;
procedure #MASM~stackPushLiveFloatRegs($fpus: #FloatRegSet)
{
  var $newPushedLiveFloatRegs: #Map #PhyFloatReg #FloatData;
  var $phyFpus: #Set #PhyFloatReg;

  $newPushedLiveFloatRegs := #MASM~stackPushLiveFloatRegs~raw(
    #MASM~pushedLiveFloatRegs, #MASM~floatRegs, $fpus
  );

  assert (forall r:#FloatReg ::
    #Set~contains(#FloatRegSet~rawSet($fpus), r) ==>
      (#FloatReg^field~type(r) ==
        #FloatData~contentType(#Map~get(#MASM~floatRegs, #FloatReg^field~reg(r)))));

  assume (forall p:#PhyFloatReg ::
    (exists r:#FloatReg ::
        (#FloatReg^field~reg(r) == p && #Set~contains(#FloatRegSet~rawSet($fpus), r))) ==>
      (#Map~get($newPushedLiveFloatRegs, p) == #Map~get(#MASM~floatRegs, p)));
  assume (forall p:#PhyFloatReg ::
    (!(exists r:#FloatReg ::
        (#FloatReg^field~reg(r) == p && #Set~contains(#FloatRegSet~rawSet($fpus), r)))) ==>
      (#Map~get($newPushedLiveFloatRegs, p) == #Map~get(#MASM~pushedLiveFloatRegs, p)));

  #MASM~pushedLiveFloatRegs := $newPushedLiveFloatRegs;
}

//procedure #MASM~stackPopLiveFloatReg($floatReg: #FloatReg)
//{
//    var $data: #FloatData;
//    $data := #Map~get(#MASM~pushedLiveFloatRegs, #FloatReg^field~reg($floatReg));
//    assert #FloatReg^field~type($floatReg) == #FloatData~contentType($data);
//    call #MASM~setFloatData(#FloatReg^field~reg($floatReg), $data);
//}
function #MASM~stackPopLiveFloatRegs~raw(
  $pushedLiveFloatRegs: #Map #PhyFloatReg #FloatData,
  $floatRegs: #Map #PhyFloatReg #FloatData,
  #fpus: #FloatRegSet,
  #fpusIgnore: #FloatRegSet
): #Map #PhyFloatReg #FloatData;
procedure #MASM~stackPopLiveFloatRegs($fpus: #FloatRegSet, $fpusIgnore: #FloatRegSet)
{
  var $newFloatRegs: #Map #PhyFloatReg #FloatData;

  $newFloatRegs := #MASM~stackPopLiveFloatRegs~raw(
    #MASM~floatRegs, #MASM~pushedLiveFloatRegs, $fpus, $fpusIgnore
  );

  assert (forall r:#FloatReg ::
    (#Set~contains(#FloatRegSet~rawSet($fpus), r) &&
        !#Set~contains(#FloatRegSet~rawSet($fpusIgnore), r)) ==>
      (#FloatReg^field~type(r) ==
        #FloatData~contentType(#Map~get(#MASM~pushedLiveFloatRegs, #FloatReg^field~reg(r)))));

  assume (forall p:#PhyFloatReg ::
    (exists r:#FloatReg ::
        (#FloatReg^field~reg(r) == p &&
          #Set~contains(#FloatRegSet~rawSet($fpus), r) &&
          !#Set~contains(#FloatRegSet~rawSet($fpusIgnore), r))) ==>
      (#Map~get($newFloatRegs, p) == #Map~get(#MASM~pushedLiveFloatRegs, p)));
  assume (forall p:#PhyFloatReg ::
    (!(exists r:#FloatReg ::
        (#FloatReg^field~reg(r) == p &&
          #Set~contains(#FloatRegSet~rawSet($fpus), r) &&
          !#Set~contains(#FloatRegSet~rawSet($fpusIgnore), r)))) ==>
      (#Map~get($newFloatRegs, p) == #Map~get(#MASM~floatRegs, p)));

  #MASM~floatRegs := $newFloatRegs;
}

var #MASM~stack: #Map #UInt64 #StackData;

procedure #MASM~stackPush($data: #StackData)
    modifies #MASM~regs, #MASM~stack;
{
    var $stackPtr: #UInt64;
    var $dataSize: #UInt64;

    call $stackPtr := #MASM~getStackIndex(#Reg^Variant~Rsp());
    call $dataSize := #StackData~sizeOf($data);
    $stackPtr := #UInt64^sub($stackPtr, $dataSize);
    assert #UInt64^gte($stackPtr, 0bv64);
    call #MASM~setStackIndex(#Reg^Variant~Rsp(), $stackPtr);
    #MASM~stack := #Map~set(#MASM~stack, $stackPtr, $data);
}

procedure #MASM~stackPop()
    returns (ret: #StackData)
    modifies #MASM~regs, #MASM~stack;
{
    var $stackPtr: #UInt64;
    var $dataSize: #UInt64;

    call $stackPtr := #MASM~getStackIndex(#Reg^Variant~Rsp());
    ret := #Map~get(#MASM~stack, $stackPtr);
    call $dataSize := #StackData~sizeOf(ret);
    $stackPtr := #UInt64^add($stackPtr, $dataSize);
    call #MASM~setStackIndex(#Reg^Variant~Rsp(), $stackPtr);
}

procedure #MASM~stackStore($idx: #UInt64, $data: #StackData)
    modifies #MASM~stack;
{
    #MASM~stack := #Map~set(#MASM~stack, $idx, $data);
}

procedure #MASM~stackLoad($idx: #UInt64)
    returns (ret: #StackData)
{
    ret := #Map~get(#MASM~stack, $idx);
}

function #MoveResolver~regMoves($resolver: #MoveResolver): #Map int #RegData;
function #MoveResolver~regCount($resolver: #MoveResolver): int;
function #MoveResolver~floatMoves($resolver: #MoveResolver): #Map int #FloatData;
function #MoveResolver~floatCount($resolver: #MoveResolver): int;

function #MoveResolver~emptyUnchecked(): #MoveResolver;

procedure #MoveResolver~empty()
  returns (ret: #MoveResolver)
{
  ret := #MoveResolver~emptyUnchecked();
  assume #MoveResolver~regCount(ret) == 0;
  assume #MoveResolver~floatCount(ret) == 0;
}

function #MoveResolver~newUnchecked(
  $regCount: int, $regMoves: #Map int #RegData,
  $floatCount: int, $floatMoves: #Map int #FloatData
): #MoveResolver;

procedure #MoveResolver~new(
  $regCount: int, $regMoves: #Map int #RegData,
  $floatCount: int, $floatMoves: #Map int #FloatData
)
  returns (ret: #MoveResolver)
{
  ret := #MoveResolver~newUnchecked($regCount, $regMoves, $floatCount, $floatMoves);
  assume #MoveResolver~regCount(ret) == $regCount;
  assume #MoveResolver~regMoves(ret) == $regMoves;
  assume #MoveResolver~floatCount(ret) == $floatCount;
  assume #MoveResolver~floatMoves(ret) == $floatMoves;
}

procedure #MoveResolver~reset()
  returns ($newResolver: #MoveResolver)
{
  call $newResolver := #MoveResolver~empty();
}

procedure #MoveResolver~addRegMove($resolver: #MoveResolver, $data: #RegData)
  returns ($newResolver: #MoveResolver)
{
  var $regMoves: #Map int #RegData;
  var $regCount: int;
  var $floatMoves: #Map int #FloatData;
  var $floatCount: int;

  $regMoves := #MoveResolver~regMoves($resolver);
  $regCount := #MoveResolver~regCount($resolver);
  $floatMoves := #MoveResolver~floatMoves($resolver);
  $floatCount := #MoveResolver~floatCount($resolver);

  call $newResolver := #MoveResolver~new(
    $regCount + 1, #Map~set($regMoves, $regCount, $data),
    $floatCount, $floatMoves
  );
}

procedure #MoveResolver~addFloatMove($resolver: #MoveResolver, $data: #FloatData)
  returns ($newResolver: #MoveResolver)
{
  var $regMoves: #Map int #RegData;
  var $regCount: int;
  var $floatMoves: #Map int #FloatData;
  var $floatCount: int;

  $regMoves := #MoveResolver~regMoves($resolver);
  $regCount := #MoveResolver~regCount($resolver);
  $floatMoves := #MoveResolver~floatMoves($resolver);
  $floatCount := #MoveResolver~floatCount($resolver);

  call $newResolver := #MoveResolver~new(
    $regCount, $regMoves,
    $floatCount + 1, #Map~set($floatMoves, $floatCount, $data)
  );
}

procedure #MoveResolver~resolve($resolver: #MoveResolver)
{
  var $regMoves: #Map int #RegData;
  var $regCount: int;
  var $floatMoves: #Map int #FloatData;
  var $floatCount: int;

  $regMoves := #MoveResolver~regMoves($resolver);
  $regCount := #MoveResolver~regCount($resolver);
  $floatMoves := #MoveResolver~floatMoves($resolver);
  $floatCount := #MoveResolver~floatCount($resolver);

  if ($regCount > 0) {
    call #MASM~setData(#Reg^Variant~Rdi(), #Map~get($regMoves, 0));
  }

  if ($regCount > 1) {
    call #MASM~setData(#Reg^Variant~Rsi(), #Map~get($regMoves, 1));
  }

  if ($regCount > 2) {
    call #MASM~setData(#Reg^Variant~Rdx(), #Map~get($regMoves, 2));
  }

  if ($regCount > 3) {
    call #MASM~setData(#Reg^Variant~Rcx(), #Map~get($regMoves, 3));
  }

  if ($floatCount > 0) {
    call #MASM~setFloatData(#PhyFloatReg^Variant~Xmm0(), #Map~get($floatMoves, 0)); 
  }

  if ($floatCount > 1) {
    call #MASM~setFloatData(#PhyFloatReg^Variant~Xmm1(), #Map~get($floatMoves, 1)); 
  }
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

    assert !#CacheIR~addedFailurePath;
    assert !#CacheIR~hasAutoScratchFloatRegisterSpill;
    
    $id'0 := #OperandId~id($typedId);
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

    assert !#CacheIR~addedFailurePath;
    assert !#CacheIR~hasAutoScratchFloatRegisterSpill;
    
    $id'0 := #OperandId~id($valueId);
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
var #CacheIR~numAvailableRegs: int;

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
  var tmp'1: #Bool;
  var tmp'2: #Bool;

  assert !#CacheIR~addedFailurePath;
  assert !#CacheIR~hasAutoScratchFloatRegisterSpill;

  call tmp'0 := #CacheIR~hasAvailableReg();
  assert tmp'0;

  ret := #CacheIR~allocateRegUnchecked(#CacheIR~allocatedRegs);

  call tmp'1 := #CacheIR~isAllocatableReg(ret);
  assume tmp'1;

  call tmp'2 := #CacheIR~isAllocatedReg(ret);
  assume !tmp'2;

  #CacheIR~allocatedRegs := #Set~add(#CacheIR~allocatedRegs, ret);
  #CacheIR~numAvailableRegs := #CacheIR~numAvailableRegs - 1;
}

procedure {:inline 1} #CacheIR~allocateKnownReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  var tmp'0: #Bool;
  
  // register should not already be allocated
  // TODO(spinda): Replace this with a call to #CacheIR~isAllocatedReg?
  assert !#Set~contains(#CacheIR~allocatedRegs, $reg);

  #CacheIR~allocatedRegs := #Set~add(#CacheIR~allocatedRegs, $reg);

  call tmp'0 := #CacheIR~isAllocatableReg($reg);
  if (tmp'0) {
    #CacheIR~numAvailableRegs := #CacheIR~numAvailableRegs - 1;
  }
}

function #CacheIR~allocateRegUnchecked($allocatedRegs: #Set #Reg): #Reg;

procedure #CacheIR~releaseReg($reg: #Reg)
  modifies #CacheIR~allocatedRegs;
{
  var tmp'0: #Bool;
  var tmp'1: #Bool;

  // register should already be allocated
  tmp'0 := #Set~contains(#CacheIR~allocatedRegs, $reg);
  assert tmp'0;

  #CacheIR~allocatedRegs := #Set~remove(#CacheIR~allocatedRegs, $reg);

  call tmp'1 := #CacheIR~isAllocatableReg($reg);
  if (tmp'1) {
    #CacheIR~numAvailableRegs := #CacheIR~numAvailableRegs + 1;
  }
}

procedure #initRegState()
{
  var $uninitializedStackData: #StackData;

  #CacheIR~nextOperandId := 0bv16;
  #CacheIR~nextFieldOffset := 0bv32;

  #CacheIR~addedFailurePath := false;
  #CacheIR~hasAutoScratchFloatRegisterSpill := false;
  #CacheIR~isDoubleScratchRegAllocated := false;

  // All registers are initially unallocated.
  #CacheIR~allocatedRegs := #Set~empty(); 
  #CacheIR~numAvailableRegs := 13;
  // => rax, rbx, rcx, rdx, rsi, rdi, r8, r9, r10, r12, r13, r14, r15

  call $uninitializedStackData := #StackData~uninitialized();
  #MASM~stack := #Map~const($uninitializedStackData);

  call #MASM~setStackIndex(#Reg^Variant~Rsp(), 512bv64);

  #MASM~inABICall := false;

  call #CacheIR~liveFloatRegSet := #FloatRegSet~empty();
  #MASM~hasPushedRegs := false;
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

procedure #addLiveFloatReg($floatReg: #FloatReg)
{
  var data'0: #FloatData;

  havoc data'0;
  call #CacheIR~liveFloatRegSet := #FloatRegSet~add(#CacheIR~liveFloatRegSet, $floatReg);

  assume #FloatReg^field~type($floatReg) == #FloatData~contentType(data'0);
  call #MASM~setFloatData(#FloatReg^field~reg($floatReg), data'0);
}

procedure {:inline 1} #CacheIR~hasAvailableReg()
  returns (ret: #Bool)
{
  // True if there is at least one allocatable register that is not already
  // allocated.
  /*
  ret := !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~Rax()) ||
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
    !#Set~contains(#CacheIR~allocatedRegs, #Reg^Variant~R15());
  */
  ret := #CacheIR~numAvailableRegs > 0;
}

procedure {:inline 1} #CacheIR~isAllocatedValueReg($valueReg: #ValueReg)
  returns (ret: #Bool)
{
  call ret := #CacheIR~isAllocatedReg($valueReg);
}

procedure {:inline 1} #CacheIR~isAllocatedReg($reg: #Reg)
  returns (ret: #Bool)
{
  ret := #Set~contains(#CacheIR~allocatedRegs, $reg);
}
