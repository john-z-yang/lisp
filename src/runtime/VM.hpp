#ifndef LISP_SRC_RUNTIME_VM_HPP_
#define LISP_SRC_RUNTIME_VM_HPP_

#define LISP_GC_HEAP_GROWTH_FACTOR 2
#define LISP_GC_INIT_HEAP_SIZE 4096
#define LISP_INT_CACHE_MAX 256.0
#define LISP_INT_CACHE_MIN -16.0

#include "../sexpr/SExpr.hpp"
#include "CPPFn.hpp"
#include "CallFrame.hpp"
#include "Env.hpp"
#include "FreeStore.hpp"
#include "StackIter.hpp"
#include "Upvalue.hpp"
#include <deque>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace runtime {

class VM {
  friend CPPFn lispApply;
  friend class RuntimeError;

private:
  Env globals;

  std::vector<std::reference_wrapper<const sexpr::SExpr>> stack;
  std::vector<CallFrame> callFrames;
  std::unordered_map<StackPtr, std::shared_ptr<Upvalue>> openUpvals;

  const sexpr::SExpr &eval(const sexpr::Fn &main, bool withGC);
  const sexpr::SExpr &exec();

  void call(const uint8_t argc);
  std::shared_ptr<Upvalue> captureUpvalue(StackPtr pos);
  const sexpr::SExpr &peak(StackPtr distance);
  const sexpr::SExpr &makeList(StackIter start);
  unsigned int unpackList(const sexpr::SExpr &sexpr);
  void reset();

public:
  VM();

  FreeStore freeStore;

  const sexpr::SExpr &evalWithGC(const sexpr::Fn &main);
  const sexpr::SExpr &eval(const sexpr::Fn &main);

  void regMacro(const sexpr::Sym &sym);
  bool isMacro(const sexpr::Sym &sym);
};
} // namespace runtime

#endif
