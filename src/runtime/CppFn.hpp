#ifndef LISP_SRC_RUNTIME_NATFN_HPP_
#define LISP_SRC_RUNTIME_NATFN_HPP_

#include "../sexpr/SExpr.hpp"
#include "VM.hpp"

namespace runtime {

using CppFn =
    const sexpr::SExpr *(std::vector<const sexpr::SExpr *>::iterator params,
                         const uint8_t argc, VM &vm);

} // namespace runtime

#endif
