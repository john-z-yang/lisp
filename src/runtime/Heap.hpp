#ifndef LISP_SRC_RUNTIME_FREESTORE_HPP_
#define LISP_SRC_RUNTIME_FREESTORE_HPP_

#include "../sexpr/Bool.hpp"
#include "../sexpr/Closure.hpp"
#include "../sexpr/NatFn.hpp"
#include "../sexpr/Nil.hpp"
#include "../sexpr/Num.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/String.hpp"
#include "../sexpr/Undefined.hpp"
#include "Suballocator.hpp"
#include <deque>
#include <unordered_set>

namespace runtime {

class VM;
class GCGuard;

class Heap {
  friend GCGuard;

private:
  VM &vm;

  std::tuple<
      Suballocator<sexpr::Closure>,
      Suballocator<sexpr::NatFn>,
      Suballocator<sexpr::Num>,
      Suballocator<sexpr::Prototype>,
      Suballocator<sexpr::SExprs>,
      Suballocator<sexpr::String>,
      Suballocator<sexpr::Sym>>
      allocators;
  bool enableGC;
  size_t maxHeapSize;

  std::unordered_set<const sexpr::SExpr *> black;
  std::deque<const sexpr::SExpr *> grey;

  void gc();
  void markRoots();
  void mark(const sexpr::SExpr *sexpr);
  void trace(const sexpr::SExpr *sexpr);

public:
  static constexpr std::size_t INIT_HEAP_SIZE = 1;
  static constexpr std::size_t HEAP_GROWTH_FACTOR = 2;

  Heap(VM &vm);

  std::size_t getBytesAlloced();
  GCGuard pauseGC();

  template <typename T, typename... Args> const T *alloc(Args &&...args) {
    gc();
    return std::get<Suballocator<T>>(allocators)
        .alloc(std::forward<Args>(args)...);
  }
};
template <> inline const sexpr::Undefined *Heap::alloc() {
  return sexpr::Undefined::getInstance();
}
template <> inline const sexpr::Nil *Heap::alloc() {
  return sexpr::Nil::getInstance();
}
template <>
inline const sexpr::Bool *Heap::alloc(sexpr::Bool::ValueType &&val) {
  return sexpr::Bool::getInstance(val);
}

} // namespace runtime

#endif
