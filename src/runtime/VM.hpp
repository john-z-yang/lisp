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

  CallFrame &callFrame();
  const sexpr::Closure &closure();
  const sexpr::Fn &fn();
  const code::Code &code();

  code::InstrPtr &instPtr();
  runtime::StackPtr &basePtr();

  const sexpr::SExpr &readConst();
  uint8_t readByte();
  uint16_t readShort();

  void call(const uint8_t argc);
  std::shared_ptr<Upvalue> captureUpvalue(StackPtr pos);
  const sexpr::SExpr &peak(StackPtr distance);
  const sexpr::SExpr &makeList(StackIter start);
  unsigned int unpackList(const sexpr::SExpr &sexpr);
  void reset();

  const sexpr::SExpr &exec();

public:
  VM();

  FreeStore freeStore;

  void regMacro(const sexpr::Sym &sym);
  bool isMacro(const sexpr::Sym &sym);

  const sexpr::SExpr &evalWithGC(const sexpr::Fn &main);
  const sexpr::SExpr &eval(const sexpr::Fn &main);
};
} // namespace runtime

#endif
