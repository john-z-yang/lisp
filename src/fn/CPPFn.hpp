#ifndef LISP_SRC_FN_CPPFN_HPP_
#define LISP_SRC_FN_CPPFN_HPP_

#include "../runtime/StackIter.hpp"
#include "../sexpr/SExpr.hpp"

namespace runtime {

class VM;

} // namespace runtime

namespace fn {

using CPPFn = const sexpr::SExpr &(runtime::StackIter params,
                                   const uint8_t argc, runtime::VM &vm);

} // namespace fn

#endif
