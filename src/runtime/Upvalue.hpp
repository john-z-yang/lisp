#ifndef LISP_SRC_VM_UPVALUE_HPP_
#define LISP_SRC_VM_UPVALUE_HPP_

#include "../common/sexpr/SExpr.hpp"
#include <memory>

struct Upvalue {
  std::shared_ptr<SExpr> *ptr;
  std::shared_ptr<SExpr> closed;
};

#endif
