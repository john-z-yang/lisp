#ifndef LISP_SRC_RUNTIME_HEAP_HPP_
#define LISP_SRC_RUNTIME_HEAP_HPP_

#include "../sexpr/Bool.hpp"
#include "../sexpr/Closure.hpp"
#include "../sexpr/NatFn.hpp"
#include "../sexpr/Nil.hpp"
#include "../sexpr/Num.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/String.hpp"
#include "../sexpr/Sym.hpp"
#include "../sexpr/Undefined.hpp"
#include "BreakTable.hpp"
#include "Suballocator.hpp"
#include <cmath>
#include <deque>
#include <functional>
#include <unordered_set>

namespace runtime {

class VM;
class GCGuard;

class Heap {
  friend GCGuard;

private:
  VM &vm;

  bool enableGC;

  std::tuple<
      Suballocator<sexpr::Closure>,
      Suballocator<sexpr::NatFn>,
      Suballocator<sexpr::Num>,
      Suballocator<sexpr::Prototype>,
      Suballocator<sexpr::SExprs>,
      Suballocator<sexpr::String>,
      Suballocator<sexpr::Sym>,
      Suballocator<sexpr::Upvalue>>
      allocators;

  std::size_t maxHeapSize;
  std::unordered_set<sexpr::SExpr *> black;
  std::deque<sexpr::SExpr *> grey;

  static constexpr auto HEAP_GROWTH_FACTOR = 2U;
  static constexpr auto INIT_MAX_HEAP_SIZE = 16777216U;

  void markRoots();
  void markSExpr(sexpr::SExpr *sexpr);
  void trace(sexpr::SExpr *sexpr);

public:
  Heap(VM &vm);

  const BreakTable gc();
  GCGuard pauseGC();

  std::size_t getBytesAlloced();

  template <typename T, typename... Args> T *alloc(Args &&...args) {
    const auto res = std::get<Suballocator<T>>(allocators)
                         .alloc(std::forward<Args>(args)...);
    grey.push_back(res);
    return static_cast<T *>(gc().get(res));
  }

  void free(
      const std::unordered_set<sexpr::SExpr *> &reachable,
      BreakTable &breakTable
  );
};

template <> inline sexpr::Bool *Heap::alloc(sexpr::Bool::ValueType &&v) {
  return sexpr::Bool::getInstance(v);
}
template <> inline sexpr::Nil *Heap::alloc() {
  return sexpr::Nil::getInstance();
}
template <> inline sexpr::Undefined *Heap::alloc() {
  return sexpr::Undefined::getInstance();
}

} // namespace runtime

#endif
