// ... begin prelude ...

type Pc = int;

type {:datatype} EmitPath;
function {:constructor} NilEmitPath(): EmitPath;
function {:constructor} ConsEmitPath(init: EmitPath, last: int): EmitPath;

type #Unit;

const #unit: #Unit;

axiom (forall x: #Unit, y: #Unit :: x == y);

type #Bool = bool;

type #Int32 = bv32;

function {:bvbuiltin "bvadd"} #Int32^Add(x: #Int32, y: #Int32): #Int32;
function {:bvbuiltin "bvneg"} #Int32^Negate(n: #Int32): #Int32;
function {:bvbuiltin "bvsgt"} #Int32^GreaterThan(a: #Int32, y: #Int32): #Bool;
function {:bvbuiltin "bvslt"} #Int32^LessThan(a: #Int32, y: #Int32): #Bool;
function {:bvbuiltin "bvsge"} #Int32^GreaterThanOrEqual(a: #Int32, y: #Int32): #Bool;
function {:bvbuiltin "bvsle"} #Int32^LessThanOrEqual(a: #Int32, y: #Int32): #Bool;

// etc; see https://boogie-docs.readthedocs.io/en/latest/LangRef.html#other-operators

type #Double = float53e11; // 64-bit; see https://github.com/boogie-org/boogie/issues/29#issuecomment-231239065

type #Map k v = [k]v;

function {:inline} #Map~get<k, v>(map: #Map k v, key: k): v {
  map[key]
}

function {:inline} #Map~set<k, v>(map: #Map k v, key: k, value: v): #Map k v {
  map[key := value]
}

type #Set a = #Map a #Bool;

function {:inline} #Set~contains<a>(set: #Set a, value: a): #Bool {
  #Map~get(set, value)
}

function {:inline} #Set~add<a>(set: #Set a, value: a): #Set a {
  #Map~set(set, value, true)
}

function {:inline} #Set~remove<a>(set: #Set a, value: a): #Set a {
  #Map~set(set, value, false)
}

// ... end prelude ...
