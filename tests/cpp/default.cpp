#include <variant>
#include <cassert>

#define Cachet_Assert assert

using Cachet_ContextRef = std::monostate;


#include <cpp_prelude.h>
using namespace cachet::prelude;

#include <test.h>
#include <test.inc>

int main () {
    Fn_test(std::monostate());
}