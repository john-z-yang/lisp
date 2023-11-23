#ifndef LISP_SRC_RUNTIME_VM_HPP_
#define LISP_SRC_RUNTIME_VM_HPP_

#include "../fn/CPPFn.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/Upvalue.hpp"
#include "BreakTable.hpp"
#include "CallFrame.hpp"
#include "Env.hpp"
#include "Heap.hpp"
#include "StackIter.hpp"
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

class Heap;

class VM {
  friend fn::CPPFn fn::apply;
  friend class RuntimeError;

private:
  code::InstrPtr ip;
  runtime::StackPtr bp;
  std::optional<sexpr::Closure *> closure;

  std::vector<sexpr::SExpr *> stack;
  std::vector<CallFrame> callFrames;
  std::unordered_map<StackPtr, sexpr::Upvalue *> openUpvals;

  sexpr::SExpr *readConst();
  uint8_t readByte();
  uint16_t readShort();

  void call(const uint8_t argc);
  sexpr::Upvalue *captureUpvalue(StackPtr pos);
  sexpr::SExpr *peak(StackPtr distance);
  sexpr::SExpr *makeList(StackIter start);
  unsigned int unpackList(sexpr::SExpr *sexpr);
  void reset();

  sexpr::SExpr *exec();

public:
  VM();

  Heap heap;
  Env env;

  void load(sexpr::Prototype *main);
  sexpr::SExpr *eval();

  const std::optional<sexpr::Closure *> &getClosure() const;
  const std::vector<sexpr::SExpr *> &getStack() const;
  const std::vector<CallFrame> &getCallFrames() const;
  const std::unordered_map<StackPtr, sexpr::Upvalue *> &getOpenUpvals() const;
  const Env::SymTable &getSymTable() const;

  void fixupAddrs(const BreakTable &breakTable);
};

} // namespace runtime

#endif
