#include <iostream>
#include <cstdint>
#include <limits>
#include <assert.h>
#include <variant>

using Cachet_ContextRef = std::monostate;

#include <cpp_prelude.h>
using namespace cachet::prelude;

#include <test.h>
#include <test.inc>

int main() {
    std::monostate ctx;

    int32_t i32max = std::numeric_limits<int32_t>::max();
    int64_t i64max = std::numeric_limits<int64_t>::max();
    uint16_t u16max = std::numeric_limits<uint16_t>::max();
    uint64_t u64max = std::numeric_limits<uint64_t>::max();

    assert(Fn_negate_i32(ctx, i32max) == i32max * -1);
    assert(Fn_negate_i64(ctx, i64max) == i64max * -1);

    assert (Fn_add_i32(ctx, 5, 6) == 5 + 6);
    assert (Fn_add_i64(ctx, 5, 6) == 5 + 6);
    assert (Fn_add_u16(ctx, 5, 6) == 5 + 6);
    assert (Fn_add_u64(ctx, 5, 6) == 5 + 6);


    assert (Fn_add_i32(ctx, i32max, 1) == i32max + 1);
    assert (Fn_add_i64(ctx, i64max, 1) == i64max + 1);
    assert (Fn_add_u16(ctx, u16max, 1) == static_cast<uint16_t>(u16max + 1));
    assert (Fn_add_u64(ctx, u64max, 1) == u64max + 1);


    assert (Fn_double_from_i32(ctx, 3) == static_cast<double>(static_cast<int32_t>(3)));

    assert (Fn_is_nan(ctx, std::numeric_limits<double>::quiet_NaN()));
    return 0;
}
