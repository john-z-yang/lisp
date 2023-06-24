#ifndef LISP_SRC_RUNTIME_STACKITER_HPP_
#define LISP_SRC_RUNTIME_STACKITER_HPP_

#include "../sexpr/SExpr.hpp"
#include <vector>

namespace runtime {

using StackIter = std::vector<const sexpr::SExpr *>::iterator;

} // namespace runtime

#endif
