#ifndef LISP_SRC_RUNTIME_ALLOCATOR_HPP_
#define LISP_SRC_RUNTIME_ALLOCATOR_HPP_

#include "../sexpr/Num.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/Sym.hpp"
#include "BreakTable.hpp"
#include <algorithm>
#include <cstddef>
#include <iostream>
#include <list>
#include <math.h>
#include <numeric>
#include <ostream>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace runtime {

template <class T> class Suballocator {
private:
  std::list<std::vector<T>> arenas;
  std::size_t bytesAlloced;

public:
  static constexpr auto ARENA_SIZE = 4096;

  Suballocator() : bytesAlloced(0) {}

  std::size_t getBytesAlloced() { return bytesAlloced; }

  template <typename... Args> T *alloc(Args &&...args) {
    if (arenas.empty() || arenas.back().size() == arenas.back().capacity()) {
      arenas.emplace_back();
      arenas.back().reserve(ARENA_SIZE / sizeof(T));
    }

    bytesAlloced += sizeof(T);
    return &arenas.back().emplace_back(std::forward<Args>(args)...);
  }

  void free(
      const std::unordered_set<sexpr::SExpr *> &reachable,
      BreakTable &breakTable
  ) {
    std::unordered_map<sexpr::SExpr::ID, T *> prevAddrs;
    for (auto &arena : arenas) {
      for (auto &sExpr : arena) {
        prevAddrs.insert(std::make_pair(sExpr.id, &sExpr));
      }
    }

    for (auto &arena : arenas) {
      const auto numFreed = std::erase_if(arena, [&](auto &sExpr) {
        return !reachable.contains(&sExpr);
      });
      bytesAlloced -= sizeof(T) * numFreed;
    }
    std::erase_if(arenas, [](auto arena) { return arena.empty(); });

    std::unordered_map<sexpr::SExpr::ID, T *> curAddrs;
    for (auto &arena : arenas) {
      for (auto &sExpr : arena) {
        curAddrs.insert(std::make_pair(sExpr.id, &sExpr));
      }
    }

    for (auto &[id, addr] : curAddrs) {
      breakTable.insert(prevAddrs.at(id), addr);
    }
  }

  void fixupAddrs(const BreakTable &breakTable) {
    for (auto &arena : arenas) {
      for (auto &sExpr : arena) {
        sExpr.fixupAddrs(breakTable);
      }
    }
  }
};

template <> class Suballocator<sexpr::Sym> {
private:
  std::list<std::vector<sexpr::Sym>> arenas;
  std::unordered_map<sexpr::Sym::ValueType, sexpr::Sym *> memo;

public:
  static constexpr auto ARENA_SIZE = 524288;

  std::size_t getBytesAlloced() { return memo.size() * sizeof(sexpr::Sym); }

  sexpr::Sym *alloc(const sexpr::Sym::ValueType v) {
    if (memo.contains(v)) {
      return memo.at(v);
    }

    if (arenas.empty() || arenas.back().size() == arenas.back().capacity()) {
      arenas.emplace_back();
      arenas.back().reserve(ARENA_SIZE / sizeof(sexpr::Sym));
    }

    memo.insert(std::make_pair(v, &arenas.back().emplace_back(v)));
    return memo.at(v);
  }

  void free(
      const std::unordered_set<sexpr::SExpr *> &reachable,
      BreakTable &breakTable
  ) {
    std::erase_if(memo, [&](auto &it) {
      return !reachable.contains(it.second);
    });

    std::unordered_map<sexpr::SExpr::ID, sexpr::Sym *> prevAddrs;
    for (auto &arena : arenas) {
      for (auto &sExpr : arena) {
        prevAddrs.insert(std::make_pair(sExpr.id, &sExpr));
      }
    }

    for (auto &arena : arenas) {
      std::erase_if(arena, [&](auto &sym) {
        if (!reachable.contains(&sym)) {
          memo.erase(sym.val);
          return true;
        }
        return false;
      });
    }
    std::erase_if(arenas, [](auto arena) { return arena.empty(); });

    std::unordered_map<sexpr::SExpr::ID, sexpr::Sym *> curAddrs;
    for (auto &arena : arenas) {
      for (auto &sym : arena) {
        curAddrs.insert(std::make_pair(sym.id, &sym));
      }
    }

    for (auto &[id, addr] : curAddrs) {
      breakTable.insert(prevAddrs.at(id), addr);
    }

    for (auto &[_, sym] : memo) {
      sym = static_cast<sexpr::Sym *>(breakTable.get(sym));
    }
  }

  void fixupAddrs(const BreakTable &) {}
};

template <> class Suballocator<sexpr::Num> {
private:
  std::list<std::vector<sexpr::Num>> arenas;
  std::vector<sexpr::Num> cache;
  std::size_t bytesAlloced;

public:
  static constexpr auto ARENA_SIZE = 524288;
  static constexpr sexpr::Num::ValueType CACHE_MIN_VAL = -256.0;
  static constexpr sexpr::Num::ValueType CACHE_MAX_VAL = 512.0;

  Suballocator() : bytesAlloced(0) {
    cache.reserve(CACHE_MAX_VAL - CACHE_MIN_VAL + 1);
    for (auto i{CACHE_MIN_VAL}; i <= CACHE_MAX_VAL; ++i) {
      cache.emplace_back(i);
    }
  }

  std::size_t getBytesAlloced() { return bytesAlloced; }

  sexpr::Num *alloc(const sexpr::Num::ValueType v) {
    if (v >= CACHE_MIN_VAL && v <= CACHE_MAX_VAL && floor(v) == v) {
      return &cache[v - CACHE_MIN_VAL];
    }

    if (arenas.empty() || arenas.back().size() == arenas.back().capacity()) {
      arenas.emplace_back();
      arenas.back().reserve(ARENA_SIZE / sizeof(sexpr::Num));
    }

    bytesAlloced += sizeof(sexpr::Num);
    return &arenas.back().emplace_back(v);
  }

  void free(
      const std::unordered_set<sexpr::SExpr *> &reachable,
      BreakTable &breakTable
  ) {
    std::unordered_map<sexpr::SExpr::ID, sexpr::Num *> prevAddrs;
    for (auto &arena : arenas) {
      for (auto &sExpr : arena) {
        prevAddrs.insert(std::make_pair(sExpr.id, &sExpr));
      }
    }

    for (auto &arena : arenas) {
      const auto numFreed = std::erase_if(arena, [&](auto &sExpr) {
        return !reachable.contains(&sExpr);
      });
      bytesAlloced -= sizeof(sexpr::Num) * numFreed;
    }
    std::erase_if(arenas, [](auto arena) { return arena.empty(); });

    std::unordered_map<sexpr::SExpr::ID, sexpr::Num *> curAddrs;
    for (auto &arena : arenas) {
      for (auto &sExpr : arena) {
        curAddrs.insert(std::make_pair(sExpr.id, &sExpr));
      }
    }

    for (auto &[id, addr] : curAddrs) {
      breakTable.insert(prevAddrs.at(id), addr);
    }
  }

  void fixupAddrs(const BreakTable &) {}
};

} // namespace runtime

#endif
