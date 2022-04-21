#include <cpp_prelude.h>

using namespace cachet::prelude;

using Cachet_ContextRef = std::monostate;

#include <test.h>
#include <test.inc>

int main() {
    std::monostate ctx;
    return Fn_foo(ctx, 6) ? 0 : 1;
}
