(declare-datatypes () ((ValueType
  ValueType-double
  ValueType-int32
  ValueType-bool
  ValueType-undefined
  ValueType-null
  ValueType-magic
  ValueType-string
  ValueType-symbol
  ValueType-privateGCThing
  ValueType-bigInt
  ValueType-object
  ValueType-unknown
)))

(declare-sort Value)

(declare-fun Value-typeOf (Value) ValueType)

(define-fun Value-isObject
    ((value Value))
    Bool
  (= (Value-typeOf value) ValueType-object)
)

(define-fun Value-toObject
    ((value Value))
    (Result Object)
  (if (Value-isObject value)
    (Value-toObjectUnchecked value)
    (as bail (Result Object))
  )
)

(declare-fun Value-toObjectUnchecked (Value) Object)

(declare-sort Object)

(declare-fun Object-shapeOf (Object) Shape)

(define-fun Object-toNativeObject
    ((object Object))
    (Result NativeObject)
  (let ((shape (Object-shapeOf object)))
    (let ((class (Shape-classOf shape)))
      (if (Class-isNativeObject class)
        (ok (NativeObject--from-Object object))
        (as bail (Result NativeObject))
      )
    )
  )
)

(define-fun Object-getFixedSlot
    ((object Object) (slot Int32))
    (Result Value)
  (let ((Object-toNativeObject--result (Object-toNativeObject object)))
    (if (is-ok Object-toNativeObject--result)
      (let ((nativeObject (unwrap-ok Object-toNativeObject--result)))
        (NativeObject--getFixedSlot nativeObject slot)
      )
      (as Object-toNativeObject--result (Result Value))
    )
  )
)

(define-sort NativeObject () Object)

(define-fun NativeObject-getFixedSlot
    ((nativeObject NativeObject) (slot Int32))
    (Result Value)
  (let ((shape (Object-shapeOf nativeObject)))
    (if (Shape-hasFixedSlot shape slot)
      (NativeObject-getFixedSlotUnchecked nativeObject slot)
      (as bail (Result Value))
    )
  )
)

(declare-fun NativeObject-getFixedSlotUnchecked (NativeObject Int32) Value)

(define-sort Shape)

(declare-fun Shape-classOf (Shape) Class)

(declare-fun Shape-hasFixedSlot (Shape Int32) Bool)

(define-sort Class)

(declare-fun Class-isNativeObject (Class) Bool)

(define-fun op--GuardToObject
    ((value Value))
    (Result Object)
  (let ((Value-toObject--result (Value-toObject value)))
    (if (is-ok Value-toObject--result)
      (let ((object (unwrap-ok Value-toObject--result)))
        (ok object)
      )
      (as Value-toObject--result (Result Object))
    )
  )
)

(define-fun op--GuardShape
    ((object Object) (shape Shape))
    (Result Unit)
  (if (= (Object-shapeOf object) shape)
    (ok unit)
    (as bail (Result Unit))
  )
)

(define-fun op--LoadFixedSlotResult
    ((object Object) (slot Int32))
    (Result Value)
  (let ((Object-getFixedSlot--result (Object-getFixedSlot object slot)))
    (if (is-bail Object-getFixedSlot--result)
      (as panic (Result Value))
      Object-getFixedSlot--result
    )
  )
)
