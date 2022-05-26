#include <variant>
#include <cassert>
#include <sstream>
#include <cstdio>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <array>
#include <iomanip>

#define Cachet_Assert assert

using Cachet_ContextRef = std::monostate;

#include "../../tests/cpp/cpp_prelude.h"

#include <js_spec.h>
#include <js_spec.inc>

static std::string JS_INTERP("./js_spec.js");
std::string exec(const char* cmd) {
    std::array<char, 128> buffer;
    std::string result;
    std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd, "r"), pclose);
    if (!pipe) {
        throw std::runtime_error("popen() failed!");
    }
    while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
        result += buffer.data();
    }
    return result;
}

std::string double_to_hex(double_t d) {
    std::stringstream s;
    s << std::setfill('0') << std::setw(16) << std::hex << *reinterpret_cast<uint64_t*>(&d);
    auto out = s.str();
    return out;
}

double_t hex_to_double(std::string hexstr) {
    uint64_t num = std::stoull(hexstr, nullptr, 16);

    return *reinterpret_cast<double_t*>(&num);
}


void node_unop(double_t expected, char* op, double_t x) {
    auto cmd = JS_INTERP + " " + op + " " + double_to_hex(x);
    auto result = hex_to_double(exec(cmd.c_str()));
    if (result != expected && !(std::isnan(result) && std::isnan(expected))) {
        std::cerr << "Mismatch for " << op << "(" << double_to_hex(x) << ")" << std::endl
                    << "Cachet result: " << double_to_hex(expected) << std::endl
                    << "Node result: " << double_to_hex(result) << std:: endl;
        exit(1);
    }
}

double node_binop(double_t expected, char* op, double_t x, double_t y) {
    auto cmd = JS_INTERP + " " + op + " " + double_to_hex(x) + " " + double_to_hex(y);
    return hex_to_double(exec(cmd.c_str()));
}

void test(double_t x, double_t y) {
    std::monostate ctx;

    // use an m4 macro to generate all the function calls we need.
    include(`macros.m4')
    foreach(`OP', unops, `
        auto OP()_x = Impl_JSValue::Fn_`'OP()(ctx, x);
        node_unop(OP()_x, "OP()", x);

        auto OP()_y = Impl_JSValue::Fn_`'OP()(ctx, y);
        node_unop(OP()_y, "OP()", y);
    ')
}


extern "C" int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t Size) {
  const double *doubleData = reinterpret_cast<const double_t *>(Data);
  while (Size >= sizeof(double) * 2) {
      auto a = *(doubleData++);
      auto b = *(doubleData++);

      test(a, b);

      Size -= sizeof(double)*2;
  }
  return 0;  // Non-zero return values are reserved for future use.
}
