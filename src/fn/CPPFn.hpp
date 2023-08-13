#ifndef LISP_SRC_FN_CPPFN_HPP_
#define LISP_SRC_FN_CPPFN_HPP_

#include "../runtime/StackIter.hpp"
#include <cstdint>

namespace sexpr {

class SExpr;

}

namespace runtime {

class VM;

}

namespace fn {

using CPPFn = const sexpr::SExpr &(
    runtime::StackIter params, const uint8_t argc, runtime::VM &vm
);

}

#endif
