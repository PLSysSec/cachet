#include <iostream>
#include <cstdint>
#include <limits>
#include <assert.h>

#include <cpp_prelude.h>
using namespace cachet::prelude;

using Cachet_ContextRef = std::monostate;

#include <test.h>
#include <test.inc>

int main() {
    std::monostate ctx;

    int32_t i32max = std::numeric_limits<int32_t>::max();
    int64_t i64max = std::numeric_limits<int64_t>::max();
    // uint16_t u16max = std::numeric_limits<uint16_t>::max();

    assert(Fn_negate_i32(ctx, i32max) == i32max * -1);
    assert(Fn_negate_i64(ctx, i64max) == i64max * -1);

    // assert (add_i32(5, 6) == 5 + 6);
    // assert (add_i64(5, 6) == 5 + 6);
    // assert (add_u16(5, 6) == 5 + 6);


    // assert (add_u16(i32max, 1) == i32max + 1);
    // assert (add_u16(i64max, 1) == i64max + 1);
    // assert (add_u16(u16max, 1) == u16max + 1);


    return 0;
}
