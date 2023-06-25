#ifndef LISP_SRC_RUNTIME_FREESTORE_HPP_
#define LISP_SRC_RUNTIME_FREESTORE_HPP_

#include "../sexpr/Bool.hpp"
#include "../sexpr/Nil.hpp"
#include "../sexpr/Num.hpp"
#include "../sexpr/SExpr.hpp"
#include "CallFrame.hpp"
#include "Env.hpp"
#include "GCGuard.hpp"
#include "StackPtr.hpp"
#include "Upvalue.hpp"
#include <deque>
#include <unordered_set>

#define LISP_GC_HEAP_GROWTH_FACTOR 2
#define LISP_GC_INIT_HEAP_SIZE 4096
#define LISP_INT_CACHE_MAX 256.0
#define LISP_INT_CACHE_MIN -16.0

namespace runtime {

class FreeStore {
private:
  Env &globals;
  std::vector<std::reference_wrapper<const sexpr::SExpr>> &stack;
  std::vector<CallFrame> &callFrames;
  std::unordered_map<StackPtr, std::shared_ptr<Upvalue>> &openUpvals;

  bool enableGC;
  size_t gcHeapSize;

  std::vector<std::unique_ptr<const sexpr::SExpr>> heap;
  std::vector<std::unique_ptr<const sexpr::Num>> intCache;

  std::unordered_set<const sexpr::SExpr *> black;
  std::deque<const sexpr::SExpr *> grey;

  void gc();
  void mark(const sexpr::SExpr &sexpr);
  void trace(const sexpr::SExpr &sexpr);
  void markGlobals();
  void markStack();
  void markCallFrames();
  void markOpenUpvalues();

public:
  FreeStore(Env &globals,
            std::vector<std::reference_wrapper<const sexpr::SExpr>> &stack,
            std::vector<CallFrame> &callFrames,
            std::unordered_map<StackPtr, std::shared_ptr<Upvalue>> &openUpvals);

  GCGuard startGC();
  GCGuard pauseGC();

  template <typename T, typename... Args> const T &alloc(Args &&...args) {
    gc();
    auto unique = std::make_unique<const T>(std::forward<Args>(args)...);
    const auto &ref = *unique.get();
    heap.emplace_back(std::move(unique));
    return ref;
  }
};

template <> inline const sexpr::Nil &FreeStore::alloc() {
  return sexpr::Nil::getInstance();
}
template <> inline const sexpr::Bool &FreeStore::alloc(bool &&val) {
  return sexpr::Bool::getInstance(val);
}
template <> inline const sexpr::Num &FreeStore::alloc(double &val) {
  if (val >= LISP_INT_CACHE_MIN && val <= LISP_INT_CACHE_MAX &&
      floor(val) == val) {
    return *intCache.at(val - LISP_INT_CACHE_MIN).get();
  }
  gc();
  auto unique = std::make_unique<const sexpr::Num>(val);
  const auto &ref = *unique;
  heap.emplace_back(std::move(unique));
  return ref;
}
template <> inline const sexpr::Num &FreeStore::alloc(double &&val) {
  return alloc<sexpr::Num>(val);
}

} // namespace runtime

#endif
