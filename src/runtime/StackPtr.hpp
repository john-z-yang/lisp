#ifndef LISP_SRC_RUNTIME_STACKPTR_HPP_
#define LISP_SRC_RUNTIME_STACKPTR_HPP_

#include "../sexpr/SExpr.hpp"
#include <functional>
#include <vector>

namespace runtime {

using StackPtr =
    std::vector<std::reference_wrapper<const sexpr::SExpr>>::size_type;

}

#endif
