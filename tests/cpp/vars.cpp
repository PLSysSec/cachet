#include <cstdlib>
#include <cpp_prelude.h>
using namespace cachet::prelude;

using Cachet_ContextRef = std::monostate;

#include <test.h>
#include <test.inc>

void Cachet_Assert(bool cond) {
    if (!cond) {
        exit(1);
    }
}

int main() {
    std::monostate ctx;
    Fn_test(ctx);

    return 0;
}
