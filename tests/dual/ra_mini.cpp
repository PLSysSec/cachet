#include <cassert>

#define Cachet_Assert assert

#include <cpp_prelude.h>
#include <stdio.h>

using namespace cachet::prelude;

using Cachet_ContextRef = std::monostate;

struct Range {
    int32_t lower_;
    int32_t upper_;
    bool hasInt32LowerBound_;
    bool hasInt32UpperBound_;
    bool canHaveFractionalPart_;
    bool canBeNegativeZero_;
    uint16_t max_exponent_;
};

using JSVal = double;
using Type_JSVal = PrimitiveType<JSVal*>;
using Type_Range = PrimitiveType<Range*>;

Type_Range::Val Fn_mk_range_raw(
    std::monostate ctx,
    int32_t lower_,
    int32_t upper_,
    bool hasInt32LowerBound_,
    bool hasInt32UpperBound_,
    bool canHaveFractionalPart_,
    bool canBeNegativeZero_,
    uint16_t max_exponent_
) {
    Range* r = new Range();
    r->lower_ = lower_;
    r->upper_ = upper_;
    r->hasInt32LowerBound_ = hasInt32LowerBound_;
    r->hasInt32UpperBound_ = hasInt32UpperBound_;
    r->canBeNegativeZero_ = canBeNegativeZero_;
    r->canHaveFractionalPart_ = canHaveFractionalPart_;
    r->max_exponent_ = max_exponent_;
    return r;
}

#include <test.h>
#include <test.inc>

int main() {
    std::monostate ctx;
    Fn_test(ctx);
    return 0;
}
