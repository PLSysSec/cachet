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

var #CacheIR~allocatedRegs: #Set #Reg;

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

var #MASM~floatRegs: #Map #FloatReg #Double;

procedure #MASM~getDouble($floatReg: #FloatReg)
  returns (ret: #Double)
{
  ret := #Map~get(#MASM~floatRegs, $floatReg);
}

procedure #MASM~setDouble($floatReg: #FloatReg, $double: #Double)
  modifies #MASM~floatRegs;
{
  #MASM~floatRegs := #Map~set(#MASM~floatRegs, $floatReg, $double);
}

var #CacheIR~allocatedFloatRegs: #Set #FloatReg;

procedure #CacheIR~allocateFloatReg()
  returns (ret: #FloatReg)
  modifies #CacheIR~allocatedFloatRegs;
{
  var $floatReg: #FloatReg;
  var tmp'0: #Bool;
  $floatReg := #CacheIR~allocateFloatRegUnchecked(#CacheIR~allocatedFloatRegs);
  tmp'0 := #Set~contains(#CacheIR~allocatedFloatRegs, $floatReg);
  assume !tmp'0;
  #CacheIR~allocatedFloatRegs := #Set~add(#CacheIR~allocatedFloatRegs, $floatReg);
  ret := $floatReg;
}

function #CacheIR~allocateFloatRegUnchecked($allocatedFloatRegs: #Set #FloatReg): #FloatReg;

procedure #CacheIR~releaseFloatReg($floatReg: #FloatReg)
  modifies #CacheIR~allocatedFloatRegs;
{
  #CacheIR~allocatedFloatRegs := #Set~remove(#CacheIR~allocatedFloatRegs, $floatReg);
}
