type $ValueReg = $Reg;

var $MASM$regs: $Map $Reg $Value;
    
procedure $MASM$getValue($valueReg'p: $ValueReg, frame: int)
  returns (ret: $Value)
{
  ret := $Map$get($MASM$regs, $valueReg'p);
}

procedure $MASM$setValue($valueReg'p: $ValueReg, $value'p: $Value^Ref, frame: int)
  modifies $MASM$regs;
{
  $MASM$regs := $Map$set($MASM$regs, $valueReg'p, Struct^refs[$value'p]);
}

procedure $MASM$getInt32($reg'p: $Reg, frame: int)
  returns (ret: $Int32)
{
  var tmp'0: $Value;
  call tmp'0 := $MASM$getValue($reg'p, frame + 1);
  Struct^refs[$Value^Ref^Local(frame, 0)] := tmp'0;
  call ret := $Value$toInt32($Value^Ref^Local(frame, 0), frame + 1);
}

procedure $MASM$setInt32($reg'p: $Reg, $int32'p: $Int32, frame: int)
  modifies $MASM$regs;
{
  var tmp'0: $Value;
  call tmp'0 := $Value$fromInt32($int32'p, frame + 1);
  Struct^refs[$Value^Ref^Local(frame, 0)] := tmp'0;
  call $MASM$setValue($reg'p, $Value^Ref^Local(frame, 0), frame + 1);
}

procedure $MASM$getBool($reg'p: $Reg, frame: int)
  returns (ret: $Bool)
{
  var tmp'0: $Value;
  call tmp'0 := $MASM$getValue($reg'p, frame + 1);
  Struct^refs[$Value^Ref^Local(frame, 0)] := tmp'0;
  call ret := $Value$toBool($Value^Ref^Local(frame, 0), frame + 1);
}

procedure $MASM$setBool($reg'p: $Reg, $bool'p: $Bool, frame: int)
  modifies $MASM$regs;
{
  var tmp'0: $Value;
  call tmp'0 := $Value$fromBool($bool'p, frame + 1);
  Struct^refs[$Value^Ref^Local(frame, 0)] := tmp'0;
  call $MASM$setValue($reg'p, $Value^Ref^Local(frame, 0), frame + 1);
}

procedure $MASM$getObject($reg'p: $Reg, frame: int)
  returns (ret: $Object)
{
  var tmp'0: $Value;
  call tmp'0 := $MASM$getValue($reg'p, frame + 1);
  Struct^refs[$Value^Ref^Local(frame, 0)] := tmp'0;
  call ret := $Value$toObject($Value^Ref^Local(frame, 0), frame + 1);
}

procedure $MASM$setObject($reg'p: $Reg, $object'p: $Object^Ref, frame: int)
  modifies $MASM$regs;
{
  var tmp'0: $Value;
  call tmp'0 := $Value$fromObject($object'p, frame + 1);
  Struct^refs[$Value^Ref^Local(frame, 0)] := tmp'0;
  call $MASM$setValue($reg'p, $Value^Ref^Local(frame, 0), frame + 1);
}

procedure $CacheIR$allocateValueReg(frame: int)
  returns (ret: $ValueReg)
  modifies $CacheIR$allocatedRegs;
{
  call ret := $CacheIR$allocateReg(frame + 1);
}

procedure $CacheIR$releaseValueReg($valueReg'p: $ValueReg, frame: int)
  modifies $CacheIR$allocatedRegs;
{
  call $CacheIR$releaseReg($valueReg'p, frame + 1);
}

var $CacheIR$allocatedRegs: $Set $Reg;

procedure $CacheIR$allocateReg(frame: int)
  returns (ret: $Reg)
  modifies $CacheIR$allocatedRegs;
{
  var $reg'v0: $Reg;
  var tmp'0: $Bool;
  $reg'v0 := $CacheIR$allocateRegUnchecked($CacheIR$allocatedRegs);
  tmp'0 := $Set$contains($CacheIR$allocatedRegs, $reg'v0);
  assume !tmp'0;
  $CacheIR$allocatedRegs := $Set$add($CacheIR$allocatedRegs, $reg'v0);
  ret := $reg'v0;
}

function $CacheIR$allocateRegUnchecked($allocatedRegs'p: $Set $Reg): $Reg;

procedure $CacheIR$releaseReg($reg'p: $Reg, frame: int)
  modifies $CacheIR$allocatedRegs;
{
  $CacheIR$allocatedRegs := $Set$remove($CacheIR$allocatedRegs, $reg'p);
}
