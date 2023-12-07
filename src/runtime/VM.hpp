#ifndef LISP_SRC_RUNTIME_VM_HPP_
#define LISP_SRC_RUNTIME_VM_HPP_

#define LISP_GC_HEAP_GROWTH_FACTOR 2
#define LISP_GC_INIT_HEAP_SIZE 4096
#define LISP_INT_CACHE_MAX 256.0
#define LISP_INT_CACHE_MIN -16.0

#include "../fn/CPPFn.hpp"
#include "../sexpr/SExpr.hpp"
#include "CallFrame.hpp"
#include "Env.hpp"
#include "Heap.hpp"
#include "StackIter.hpp"
#include "Upvalue.hpp"
#include <deque>
#include <functional>
#include <memory>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace fn {

CPPFn apply;

}

namespace runtime {

class VM {
  friend fn::CPPFn fn::apply;
  friend class RuntimeError;

private:
  code::InstrPtr ip;
  runtime::StackPtr bp;
  std::optional<const sexpr::Closure *> closure;

  std::vector<const sexpr::SExpr *> stack;
  std::vector<CallFrame> callFrames;
  std::unordered_map<StackPtr, std::shared_ptr<Upvalue>> openUpvals;

  const sexpr::SExpr *readConst();
  uint8_t readByte();
  uint16_t readShort();

  void call(const uint8_t argc);
  std::shared_ptr<Upvalue> captureUpvalue(StackPtr pos);
  const sexpr::SExpr *peak(StackPtr distance);
  const sexpr::SExpr *makeList(StackIter start);
  unsigned int unpackList(const sexpr::SExpr *sexpr);
  void reset();

  const sexpr::SExpr *exec();

public:
  VM();

  Heap heap;
  Env env;

  const sexpr::SExpr *
  eval(const sexpr::Prototype *main, bool disableGC = false);
};

} // namespace runtime

#endif
