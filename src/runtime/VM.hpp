#ifndef LISP_SRC_RUNTIME_VM_HPP_
#define LISP_SRC_RUNTIME_VM_HPP_

#define LISP_GC_HEAP_GROWTH_FACTOR 2
#define LISP_GC_INIT_HEAP_SIZE 4096
#define LISP_INT_CACHE_MAX 256.0
#define LISP_INT_CACHE_MIN -16.0

#include "../sexpr/Bool.hpp"
#include "../sexpr/Nil.hpp"
#include "../sexpr/Num.hpp"
#include "../sexpr/SExpr.hpp"
#include "CallFrame.hpp"
#include "Env.hpp"
#include "Upvalue.hpp"
#include <cmath>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace runtime {

class VM {
  friend class RuntimeError;

private:
  Env globals;

  std::vector<const sexpr::SExpr *> stack;
  std::vector<CallFrame> callFrames;
  std::unordered_map<StackPtr, std::shared_ptr<Upvalue>> openUpvals;

  bool enableGC;
  size_t gcHeapSize;

  std::vector<std::unique_ptr<const sexpr::SExpr>> heap;
  std::vector<std::unique_ptr<const sexpr::Num>> intCache;

  std::unordered_set<const sexpr::SExpr *> black;
  std::unordered_set<const sexpr::SExpr *> grey;

  const sexpr::SExpr *eval(const sexpr::Fn *main, bool withGC);
  const sexpr::SExpr *exec(const sexpr::Fn *main);

  void call(const uint8_t argc);
  std::shared_ptr<Upvalue> captureUpvalue(StackPtr pos);
  const sexpr::SExpr *peak(StackPtr distance);
  const sexpr::SExpr *makeList(StackPtr size);
  void reset();

  void gc();
  void mark(const sexpr::SExpr *sexpr);
  void trace(const sexpr::SExpr *sexpr);
  void markGlobals();
  void markStack();
  void markCallFrames();
  void markOpenUpvalues();

public:
  VM();

  const sexpr::SExpr *evalWithGC(const sexpr::Fn *main);
  const sexpr::SExpr *eval(const sexpr::Fn *main);

  void defMacro(const sexpr::Sym *sym);
  bool isMacro(const sexpr::Sym *sym);

  template <typename T, typename... Args> const T *alloc(Args &&...args) {
    if (enableGC && heap.size() > gcHeapSize) {
      gc();
      gcHeapSize = heap.size() * LISP_GC_HEAP_GROWTH_FACTOR;
    }
    auto unique = std::make_unique<const T>(std::forward<Args>(args)...);
    const auto ptr = unique.get();
    heap.emplace_back(std::move(unique));
    return ptr;
  }
};

template <> inline const sexpr::Nil *VM::alloc() {
  return sexpr::Nil::getInstance();
}
template <> inline const sexpr::Bool *VM::alloc(bool &&val) {
  return sexpr::Bool::getInstance(val);
}
template <> inline const sexpr::Num *VM::alloc(double &val) {
  if (val >= LISP_INT_CACHE_MIN && val <= LISP_INT_CACHE_MAX &&
      floor(val) == val) {
    return intCache.at(val - LISP_INT_CACHE_MIN).get();
  }
  auto unique = std::make_unique<const sexpr::Num>(val);
  const auto ptr = unique.get();
  heap.emplace_back(std::move(unique));
  return ptr;
}
template <> inline const sexpr::Num *VM::alloc(double &&val) {
  return alloc<sexpr::Num>(val);
}

} // namespace runtime

#endif
