#ifndef LISP_SRC_RUNTIME_FREESTORE_HPP_
#define LISP_SRC_RUNTIME_FREESTORE_HPP_

#include "../sexpr/Bool.hpp"
#include "../sexpr/Nil.hpp"
#include "../sexpr/Num.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/Undefined.hpp"
#include <cmath>
#include <deque>
#include <unordered_set>

#define FREESTORE_HEAP_GROWTH_FACTOR 2
#define FREESTORE_INIT_HEAP_SIZE 512
#define FREESTORE_INT_CACHE_MAX 256.0
#define FREESTORE_INT_CACHE_MIN -16.0

namespace runtime {

class VM;
class GCGuard;

class Heap {
  friend GCGuard;

private:
  VM &vm;

  bool enableGC;
  size_t gcHeapSize;

  std::vector<std::unique_ptr<const sexpr::SExpr>> heap;
  std::vector<std::unique_ptr<const sexpr::Num>> numCache;

  std::unordered_set<const sexpr::SExpr *> black;
  std::deque<const sexpr::SExpr *> grey;

  void gc();
  void markRoots();
  void mark(const sexpr::SExpr *sexpr);
  void trace(const sexpr::SExpr *sexpr);

public:
  Heap(VM &vm);

  GCGuard startGC();
  GCGuard pauseGC();

  template <typename T, typename... Args> const T *alloc(Args &&...args) {
    gc();
    auto unique = std::make_unique<const T>(std::forward<Args>(args)...);
    const auto &ref = *unique.get();
    heap.emplace_back(std::move(unique));
    return &ref;
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
template <> inline const sexpr::Num *Heap::alloc(sexpr::Num::ValueType &val) {
  if (val >= FREESTORE_INT_CACHE_MIN && val <= FREESTORE_INT_CACHE_MAX &&
      floor(val) == val) {
    return numCache.at(val - FREESTORE_INT_CACHE_MIN).get();
  }
  gc();
  auto unique = std::make_unique<const sexpr::Num>(val);
  const auto &ref = *unique;
  heap.emplace_back(std::move(unique));
  return &ref;
}
template <> inline const sexpr::Num *Heap::alloc(double &&val) {
  return alloc<sexpr::Num>(val);
}

} // namespace runtime

#endif
