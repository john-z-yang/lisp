#ifndef LISP_SRC_RUNTIME_COMMON_HPP_
#define LISP_SRC_RUNTIME_COMMON_HPP_

#include "../sexpr/SExpr.hpp"
#include <vector>

namespace runtime {

using StackPtr = std::vector<const sexpr::SExpr *>::size_type;

} // namespace runtime

#endif
