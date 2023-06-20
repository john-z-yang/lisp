#ifndef LISP_SRC_COMPILE_UPVALUE_HPP_
#define LISP_SRC_COMPILE_UPVALUE_HPP_

namespace compile {

struct Upvalue {
  int idx;
  bool isLocal;
};

} // namespace compile

#endif
