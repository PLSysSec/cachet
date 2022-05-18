#include <cstdlib>
#include <variant>

using Cachet_ContextRef = std::monostate;

inline void Cachet_Assert(bool failed) {
    exit(!failed);
}


#include <cpp_prelude.h>
using namespace cachet::prelude;

#include <test.h>
#include <test.inc>

int main () {
    Fn_test(std::monostate());
}