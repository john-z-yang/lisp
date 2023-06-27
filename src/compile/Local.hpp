#ifndef LISP_SRC_COMPILE_LOCAL_HPP_
#define LISP_SRC_COMPILE_LOCAL_HPP_

#include "../sexpr/Sym.hpp"

namespace compile {

struct Local {
  const sexpr::Sym &symbol;
  const uint8_t stackOffset;
  bool isCaptured;
};

} // namespace compile

#endif
