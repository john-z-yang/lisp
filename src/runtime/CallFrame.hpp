#ifndef LISP_SRC_RUNTIME_CALLFRAME_HPP_
#define LISP_SRC_RUNTIME_CALLFRAME_HPP_

#include "../sexpr/Closure.hpp"

namespace runtime {

struct CallFrame {
  const sexpr::Closure *closure;
  std::vector<uint8_t>::size_type ip;
  std::vector<const sexpr::SExpr *>::size_type bp;
};

} // namespace runtime

#endif
