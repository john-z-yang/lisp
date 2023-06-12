#ifndef LISP_SRC_VM_VM_HPP_
#define LISP_SRC_VM_VM_HPP_

#define LISP_GC_HEAP_GROWTH_FACTOR 2
#define LISP_GC_INIT_HEAP_SIZE 4096
#define LISP_INT_CACHE_MAX 256
#define LISP_INT_CACHE_MIN -16

#include "../common/sexpr/BoolAtom.hpp"
#include "../common/sexpr/ClosureAtom.hpp"
#include "../common/sexpr/IntAtom.hpp"
#include "../common/sexpr/NilAtom.hpp"
#include "../common/sexpr/SExpr.hpp"
#include "Env.hpp"
#include "Upvalue.hpp"
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <vector>

class VM {
  friend class RuntimeError;

private:
  struct CallFrame {
    const ClosureAtom *closure;
    std::vector<uint8_t>::size_type ip;
    std::vector<const SExpr *>::size_type bp;
  };

  Env globals;

  std::vector<const SExpr *> stack;
  std::vector<CallFrame> callFrames;
  std::unordered_map<std::vector<const SExpr *>::size_type,
                     std::shared_ptr<Upvalue>>
      openUpvalues;

  bool enableGC;
  size_t gcHeapSize;

  std::vector<std::unique_ptr<const SExpr>> heap;

  std::unordered_set<const SExpr *> black;
  std::unordered_set<const SExpr *> grey;
  std::vector<std::unique_ptr<const IntAtom>> intCache;

  const SExpr *eval(const FnAtom *main, bool withGC);
  const SExpr *exec(const FnAtom *main);

  void call(const uint8_t argc);

  std::shared_ptr<Upvalue>
  captureUpvalue(std::vector<const SExpr *>::size_type pos);
  const SExpr *peak(std::vector<const SExpr *>::size_type distance);
  const SExpr *makeList(std::vector<const SExpr *>::size_type size);

  void gc();

  void mark(const SExpr *sexpr);
  void trace(const SExpr *sexpr);

  void markGlobals();
  void markStack();
  void markCallFrames();
  void markOpenUpvalues();

  void reset();

public:
  VM();

  const SExpr *evalWithGC(const FnAtom *main);
  const SExpr *eval(const FnAtom *main);

  void defMacro(const SymAtom *sym);
  bool isMacro(const SymAtom *sym);

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

template <> inline const NilAtom *VM::alloc() { return NilAtom::getInstance(); }
template <> inline const BoolAtom *VM::alloc(bool &&val) {
  return BoolAtom::getInstance(val);
}
template <> inline const IntAtom *VM::alloc(int &&val) {
  if (val >= LISP_INT_CACHE_MIN && val <= LISP_INT_CACHE_MAX) {
    return intCache.at(val - LISP_INT_CACHE_MIN).get();
  }
  auto unique = std::make_unique<const IntAtom>(val);
  const auto ptr = unique.get();
  heap.emplace_back(std::move(unique));
  return ptr;
}

#endif
