#ifndef LISP_SRC_RUNTIME_ALLOCATOR_HPP_
#define LISP_SRC_RUNTIME_ALLOCATOR_HPP_

#include "../sexpr/NatFn.hpp"
#include "../sexpr/Num.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/Sym.hpp"
#include <algorithm>
#include <cstddef>
#include <iostream>
#include <list>
#include <math.h>
#include <memory>
#include <numeric>
#include <ostream>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace runtime {

template <class T> class Suballocator {
private:
  std::list<std::unique_ptr<const T>> handles;

public:
  std::size_t getBytesAlloced() { return sizeof(T) * handles.size(); }

  template <typename... Args> const T *alloc(Args &&...args) {
    return handles
        .emplace_back(std::make_unique<T>(std::forward<Args>(args)...))
        .get();
  }

  void free(const std::unordered_set<const sexpr::SExpr *> &reachable) {
    std::erase_if(handles, [&](const auto &handle) {
      return !reachable.contains(handle.get());
    });
  }
};

template <> class Suballocator<sexpr::Sym> {
private:
  std::list<std::unique_ptr<const sexpr::Sym>> handles;
  std::unordered_map<sexpr::Sym::ValueType, const sexpr::Sym *> memo;

public:
  std::size_t getBytesAlloced() { return memo.size() * sizeof(sexpr::Sym); }

  const sexpr::Sym *alloc(const sexpr::Sym::ValueType v) {
    if (memo.contains(v)) {
      return memo.at(v);
    }

    memo.insert(std::make_pair(
        v, handles.emplace_back(std::make_unique<sexpr::Sym>(v)).get()
    ));
    return memo.at(v);
  }

  void free(const std::unordered_set<const sexpr::SExpr *> &reachable) {
    std::erase_if(memo, [&](const auto &it) {
      return !reachable.contains(it.second);
    });
    std::erase_if(handles, [&](const auto &handle) {
      return !reachable.contains(handle.get());
    });
  }
};

template <> class Suballocator<sexpr::Num> {
private:
  std::list<std::unique_ptr<const sexpr::Num>> handles;
  std::vector<std::unique_ptr<const sexpr::Num>> cache;

public:
  static constexpr sexpr::Num::ValueType CACHE_MIN_VAL = -512.0;
  static constexpr sexpr::Num::ValueType CACHE_MAX_VAL = 512.0;

  Suballocator() {
    cache.reserve(CACHE_MAX_VAL - CACHE_MIN_VAL + 1);
    for (auto i{CACHE_MIN_VAL}; i <= CACHE_MAX_VAL; ++i) {
      cache.emplace_back(std::make_unique<sexpr::Num>(i));
    }
  }

  std::size_t getBytesAlloced() {
    return sizeof(sexpr::Num) * (handles.size() + cache.size());
  }

  const sexpr::Num *alloc(const sexpr::Num::ValueType v) {
    if (v >= CACHE_MIN_VAL && v <= CACHE_MAX_VAL && floor(v) == v) {
      return cache[v - CACHE_MIN_VAL].get();
    }

    return handles.emplace_back(std::make_unique<sexpr::Num>(v)).get();
  }

  void free(const std::unordered_set<const sexpr::SExpr *> &reachable) {
    std::erase_if(handles, [&](const auto &handle) {
      return !reachable.contains(handle.get());
    });
  }
};

template <> class Suballocator<sexpr::NatFn> {
private:
  std::list<std::unique_ptr<const sexpr::NatFn>> handles;

public:
  std::size_t getBytesAlloced() {
    return sizeof(sexpr::NatFn) * handles.size();
  }

  template <typename... Args> const sexpr::NatFn *alloc(Args &&...args) {
    return handles
        .emplace_back(std::make_unique<sexpr::NatFn>(std::forward<Args>(args)...
        ))
        .get();
  }

  void free(const std::unordered_set<const sexpr::SExpr *> &) {}
};

} // namespace runtime

#endif
