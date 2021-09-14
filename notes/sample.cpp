#include <variant>

enum class Error {
  Panic,
  Bail,
  Throw,
};

template <typename T>
struct PrimitiveType {
  using Val = T;
  using Local = T;
  using Ref = const T&;
  using OutRef = T&;

  static inline bool CompareEq(Val lhs, Val rhs) {
    return lhs == rhs;
  }

  static inline bool CompareNeq(Val lhs, Val rhs) {
    return lhs != rhs;
  }

  static inline Val ToVal(Ref ref) {
    return ref;
  }

  static inline Local ToLocal(Val&& val) {
    return val;
  }

  static inline Local ToLocal(Ref ref) {
    return ref;
  }

  static inline Ref ToRef(const Val& val) {
    return val;
  }

  static inline OutRef ToOutRef(Local& local) {
    return local;
  }

  static inline Val SetOutRef(OutRef out, Val&& in) {
    out = std::move(in);
    return out;
  }

  static inline Val SetOutRef(OutRef out, Ref in) {
    out = in;
    return out;
  }
};

template <typename T>
struct NumericType : public PrimitiveType<T> {
  static inline bool CompareLte(Val lhs, Val rhs) {
    return lhs <= rhs;
  }

  static inline bool CompareGte(Val lhs, Val rhs) {
    return lhs >= rhs;
  }

  static inline bool CompareLt(Val lhs, Val rhs) {
    return lhs < rhs;
  }

  static inline bool CompareGt(Val lhs, Val rhs) {
    return lhs > rhs;
  }
};

using Type_Unit = PrimitiveType<std::monostate>;
using Type_Bool = NumericType<bool>;
using Type_Int32 = NumericType<int32_t>;
using Type_Double = NumericType<double>;

namespace Impl_Unit {
  inline constexpr Type_Unit::Val Const_Unit = std::monostate();
};  // namespace Impl_Unit


namespace Impl_Bool {
  inline constexpr Type_Bool::Val Const_True = true;
  inline constexpr Type_Bool::Val Const_False = false;
};  // namespace Impl_Bool



#include "mozilla/Result.h"

#include "Cachet.h"

template <typename T>
using Result = mozilla::Result<T, Error>;

using Err = mozilla::GenericErrorResult<Error>;

template <typename T>
inline Result<T> Ok(const T& value) {
  return value;
}

template <typename T>
inline Result<T> Ok(T&& value) {
  return value;
}

template <typename T>
inline bool IsOk(const Result<T>& result) {
  return result.isOk();
}

template <typename T>
inline T Unwrap(const Result<T>& result) {
  return result.unwrap();
}

template <typename T>
inline T Unwrap(Result<T>&& result) {
  return result.unwrap();
}

template <typename T>
struct GCType {
  using Val = T;
  using Local = Rooted<T>;
  using Ref = Handle<T>;
  using OutRef = MutableHandle<T>;

  static inline bool CompareEq(Val lhs, Val rhs) {
    return lhs == rhs;
  }

  static inline bool CompareNeq(Val lhs, Val rhs) {
    return lhs != rhs;
  }

  static inline Val ToVal(const Local& local) {
    return local;
  }

  static inline Val ToVal(Ref ref) {
    return ref;
  }

  static inline Local ToLocal(Val&& val) {
    return Rooted(cx, std::move(val));
  }

  static inline Local ToLocal(const Local& local) {
    return Rooted(cx, local);
  }

  static inline Local ToLocal(Ref ref) {
    return Rooted(cx, ref);
  }

  static inline Ref ToRef(const Val& val) {
    return Handle::fromMarkedLocation(val);
  }

  static inline Ref ToRef(const Local& local) {
    return local;
  }

  static inline OutRef ToOutRef(Local& local) {
    return &local;
  }

  static inline Val SetOutRef(OutRef out, const Val& in) {
    out.set(in);
    return out;
  }

  static inline Val SetOutRef(OutRef out, Val&& in) {
    out.set(std::move(in));
    return out;
  }

  static inline Val SetOutRef(OutRef out, const Local& in) {
    out.set(in);
    return out;
  }

  static inline Val SetOutRef(OutRef out, Ref in) {
    out.set(in);
    return out;
  }
};

using Type_ValueType = PrimitiveType<JS::ValueType>;
using Type_Value = GCType<JS::Value>;
using Type_Object = GCType<JSObject*>;
using Type_NativeObject = GCType<NativeObject*>;
using Type_Shape = GCType<Shape*>;
using Type_Class = GCType<JSClass*>;

namespace Impl_ValueType {

inline constexpr Type_ValueType::Val Variant_Double = JS::ValueType::Double;
inline constexpr Type_ValueType::Val Variant_Int32 = JS::ValueType::Int32;
inline constexpr Type_ValueType::Val Variant_Boolean = JS::ValueType::Boolean;
inline constexpr Type_ValueType::Val Variant_Undefined = JS::ValueType::Undefined;
inline constexpr Type_ValueType::Val Variant_Null = JS::ValueType::Null;
inline constexpr Type_ValueType::Val Variant_Magic = JS::ValueType::Magic;
inline constexpr Type_ValueType::Val Variant_String = JS::ValueType::String;
inline constexpr Type_ValueType::Val Variant_Symbol = JS::ValueType::Symbol;
inline constexpr Type_ValueType::Val Variant_PrivateGCThing = JS::ValueType::PrivateGCThing;
inline constexpr Type_ValueType::Val Variant_BigInt = JS::ValueType::BigInt;
inline constexpr Type_ValueType::Val Variant_Object = JS::ValueType::Object;

};  // namespace Impl_ValueType

namespace Impl_Value {

Type_ValueType::Val Fn_typeOf(Type_Value::Ref param_value) {
  return param_value->type();
}

Type_Object::Val Fn_toObjectUnchecked(Type_Value::Ref param_value) {
  return &param_value->toObject();
}

};  // namespace Impl_Value

namespace Impl_Object {

Type_Shape::Value Fn_shapeOf(Type_Object::Ref param_object) {
  return param_object->shape();
}

};  // namespace Impl_Object

namespace Impl_NativeObject {

inline Type_NativeObject::Val From_Object(Type_Object::Ref in) {
  return in.as<NativeObject>();
}

Type_Value::Val Fn_getFixedSlotUnchecked(Type_NativeObject::Ref param_nativeObject, Type_Int32::Ref param_slot) {
  return param_nativeObject->getFixedSlot(param_slot);
}

};  // namespace Impl_NativeObject

namespace Impl_Shape {

Type_Class::Val Fn_classOf(Type_Shape::Ref param_shape) {
  return param_shape->clasp();
}

Type_Bool::Val Fn_hasFixedSlot(Type_Shape::Ref param_shape, Type_Int32::Ref param_slot) {
  return param_slot >= 0 && param_slot < param_shape->numFixedSlots();
}

};  // namespace Impl_Shape

namespace Impl_Class {

Type_Bool::Val Fn_isNativeObject(Type_Class::Ref param_class) {
  return param_class->isNativeObject();
}

};  // namespace Impl_Class

#include "CachetGenerated.h"





// Forward declarations.

namespace Impl_ValueType {

// inline constexpr Type_ValueType::Val Variant_Double;
// inline constexpr Type_ValueType::Val Variant_Int32;
// inline constexpr Type_ValueType::Val Variant_Bool;
// inline constexpr Type_ValueType::Val Variant_Undefined;
// inline constexpr Type_ValueType::Val Variant_Null;
// inline constexpr Type_ValueType::Val Variant_Magic;
// inline constexpr Type_ValueType::Val Variant_String;
// inline constexpr Type_ValueType::Val Variant_Symbol;
// inline constexpr Type_ValueType::Val Variant_PrivateGCThing;
// inline constexpr Type_ValueType::Val Variant_BigInt;
// inline constexpr Type_ValueType::Val Variant_Object;
// inline constexpr Type_ValueType::Val Variant_Unknown;

};  // namespace Impl_ValueType

namespace Impl_Value {

Type_ValueType::Val Fn_typeOf(Context cx, Type_Value::Ref param_value);

Type_Object::Val Fn_toObjectUnchecked(Context cx, Type_Value::Ref param_value);

};  // namespace Impl_Value

namespace Impl_Object {

Type_Shape::Val Fn_shapeOf(Context cx, Type_Object::Ref param_object);

};  // namespace Impl_Object

namespace Impl_NativeObject {

inline Type_NativeObject::Val From_Object(Context cx, Type_Object::Ref in);

Type_Value::Val Fn_getFixedSlotUnchecked(Context cx, Type_NativeObject::Ref param_nativeObject, Type_Int32::Ref param_slot);

};  // namespace Impl_NativeObject

namespace Impl_Shape {

Type_Class::Val Fn_classOf(Context cx, Type_Shape::Ref param_shape);

Type_Bool::Val Fn_hasFixedSlot(Context cx, Type_Shape::Ref param_shape, Type_Int32::Ref param_slot);

};  // namespace Impl_Shape

namespace Impl_Class {

Type_Bool::Val Fn_isNativeObject(Context cx, Type_Class::Ref param_class);

};  // namespace Impl_Class

// Function definitions.

namespace Impl_Value {

Type_Bool::Val Fn_isObject(Context cx, Type_Value::Ref param_value) {
  return ({
    Fn_typeOf(cx, param_value) == Impl_ValueType::Variant_Object;
  });
}

};  // namespace Impl_Value

namespace Impl_Value {

Result<Type_Object::Val> Fn_toObject(Context cx, Type_Value::Ref param_value) {
  return Ok(({
    if (!Impl_Value::Fn_isObject(cx, param_value)) {
      return Err(Error::Bail);
    }
    Impl_Value::Fn_toObjectUnchecked(cx, param_value);
  }));
}

};  // namespace Impl_Value

namespace Impl_Object {

Result<Type_NativeObject::Val> Fn_toNativeObject(Context cx, Type_Object::Ref param_object) {
  return Ok(({
    Type_Shape::Local local_shape(Type_Shape::ToLocal(cx, Impl_Object::Fn_shapeOf(cx, param_object)));
    Type_Class::Local local_class(Type_Class::ToLocal(cx, Impl_Shape::Fn_classOf(cx, local_shape)));
    if (!Impl_Class::Fn_isNativeObject(cx, Type_Class::ToRef(local_class))) {
      return Err(Error::Bail);
    }
    Impl_NativeObject::From_Object(param_object);
  }));
}

};  // namespace Impl_Object

namespace Impl_NativeObject {

Result<Type_Value::Val> Fn_getFixedSlot(Context cx, Type_NativeObject::Ref param_nativeObject, Type_Int32::Ref param_slot) {
  return Ok(({
    Type_Shape::Ref local_shape(Type_Shape::ToLocal(cx, Impl_Object::Fn_shapeOf(cx, param_nativeObject)));
    if (!Impl_Shape::Fn_hasFixedSlot(cx, Type_Shape::ToRef(local_shape), param_slot)) {
      return Err(Error::Bail);
    }
    Impl_NativeObject::Fn_getFixedSlotUnchecked(cx, param_nativeObject, param_slot);
  }));
}

};  // namespace Impl_NativeObject

namespace Impl_Object {

Result<Type_Value::Val> Fn_getFixedSlot(Context cx, Type_Object::Ref param_object, Type_Int32::Ref param_slot) {
  return ({
    Type_NativeObject::Local local_nativeObject(Type_NativeObject::ToLocal(cx, ({
      Result<Type_NativeObject::Val> result_0(Impl_Object::Fn_toNativeObject(cx, param_object));
      if (!IsOk(result_0)) {
        return Err(Error::Bail);
      }
      Unwrap(std::move(result_0));
    })));
    ({
     Result<Type_Value::Val> result_1(Impl_NativeObject::Fn_getFixedSlot(cx, Type_NativeObject::ToRef(local_nativeObject), param_slot));
     if (!IsOk(result_1)) {
       return Err(Error::Bail);
     }
     Unwrap(std::move(result_1));
    });
  });
}

};  // namespace Impl_Object

// Op definitions.

Result<Type_Unit::Val> Op_GuardToObject(Context cx, Type_Value::Ref param_value, Type_Object::OutRef param_object) {
  return Ok(({
    Type_Object::SetOutRef(param_object, ({
      Result<Type_Object::Val> result_0(Impl_Value::Fn_toObject(cx, param_value));
      if (!IsOk(result_0)) {
        return Err(Error::Bail);
      }
      Unwrap(std::move(result_0));
    }));
    Const_unit;
  }));
}

Result<Type_Unit::Val> Op_GuardShape(Context cx, Type_Object::Ref param_object, Type_Shape::Ref param_shape) {
  return Ok(({
    if (!(Impl_Object::Fn_shapeOf(cx, param_object) == param_shape)) {
      return Err(Error::Bail);
    }
    Const_unit;
  }));
}

Type_Value::Val Op_LoadFixedSlotResult(Context cx, Type_Object::Ref param_object, Type_Int32::Ref param_slot) {
  return ({
    Unwrap(Impl_Object::Fn_getFixedSlot(cx, param_object, param_slot));
  });
}
