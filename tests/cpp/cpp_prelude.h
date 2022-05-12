/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 * vim: set ts=8 sts=2 et sw=2 tw=80:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef cachet_prelude 
#define cachet_prelude 

#include <cstdint>
#include <utility>
#include <variant>
#include <cmath>

namespace cachet {

namespace prelude {

struct Cachet_Bail {};

template <typename T>
struct PrimitiveType {
  using Val = T;
  using Local = Val;
  using Ref = Val;
  using MutRef = Val&;

  template <typename U>
  static U&& ToVal(U&& x) {
    return std::forward<U>(x);
  }

  template <typename C>
  static Local EmptyLocal(C cx) {
    return T();
  }

  template <typename C, typename U>
  static U&& ToLocal(C cx, U&& x) {
    return std::forward<U>(x);
  }

  template <typename U>
  static U&& ToRef(U&& x) {
    return std::forward<U>(x);
  }

  template <typename U>
  static U&& Fields(U&& x) {
    return std::forward<U>(x);
  }

  static MutRef ToMutRef(Local& local) {
    return local;
  }

  template <typename U>
  static void SetMutRef(MutRef lhs, U&& rhs) {
    lhs = std::forward<U>(rhs);
  }

  static bool Eq(Ref lhs, Ref rhs) {
    return lhs == rhs;
  }

  static bool Neq(Ref lhs, Ref rhs) {
    return lhs != rhs;
  }
};

template <typename T>
struct NumericType : public PrimitiveType<T> {
  using Val = typename PrimitiveType<T>::Val;
  using Local = typename PrimitiveType<T>::Local;
  using Ref = typename PrimitiveType<T>::Ref;
  using MutRef = typename PrimitiveType<T>::MutRef;

  static bool Lte(Ref lhs, Ref rhs) {
    return lhs <= rhs;
  }

  static bool Gte(Ref lhs, Ref rhs) {
    return lhs >= rhs;
  }

  static bool Lt(Ref lhs, Ref rhs) {
    return lhs < rhs;
  }

  static bool Gt(Ref lhs, Ref rhs) {
    return lhs > rhs;
  }

  static Val Add(Ref lhs, Ref rhs) {
    return lhs + rhs;
  }

  static Val Sub(Ref lhs, Ref rhs) {
    return lhs - rhs;
  }
};


using Type_Unit = PrimitiveType<std::monostate>;
using Type_Bool = NumericType<bool>;
using Type_Int32 = NumericType<int32_t>;
using Type_Double = NumericType<double>;
using Type_Int64 = NumericType<int64_t>;
using Type_UInt16 = NumericType<uint16_t>;

namespace Impl_Double {
  template <typename T>
  inline Type_Double::Val Fn_from_i32(T ctx, Type_Int32::Ref v) {
    return static_cast<Type_Double::Val>(v);
  }

  template <typename T>
  inline Type_Double::Val Fn_is_nan(T ctx, Type_Double::Ref v) {
    return std::isnan(v);
  }

  template <typename T>
  inline Type_Double::Val Var_INFINITY(T ctx) {
    return std::numeric_limits<double>::infinity();
  }

  template <typename T>
  inline Type_Double::Val Var_NEG_INFINITY(T ctx) {
    return std::numeric_limits<double>::infinity();
  }
}

inline Type_Unit::Ref Var_unit() {
  return std::monostate();
}

inline Type_Bool::Ref Var_true() {
  return true;
}

inline Type_Bool::Ref Const_false() {
  return false;
}

};  // namespace prelude

};  // namespace cachet

#endif /*  cachet_prelude */
