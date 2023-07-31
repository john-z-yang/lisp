#ifndef LISP_SRC_RUNTIME_CALLFRAME_HPP_
#define LISP_SRC_RUNTIME_CALLFRAME_HPP_

#include "../code/InstrPtr.hpp"
#include "../sexpr/Closure.hpp"
#include "StackPtr.hpp"

namespace runtime {

struct CallFrame {
  const sexpr::Closure &closure;
  const runtime::StackPtr bp;
  const code::InstrPtr ip;
};

} // namespace runtime

#endif
