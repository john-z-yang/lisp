#include "Heap.hpp"
#include "../sexpr/Cast.cpp"
#include "../sexpr/SExprs.hpp"
#include "GCGuard.hpp"
#include "VM.hpp"
#include <algorithm>

using namespace sexpr;
using namespace runtime;

void Heap::gc() {
  if (!enableGC) {
    return;
  }
#ifndef GC_STRESS_TEST
  if (getBytesAlloced() < maxHeapSize) {
    return;
  }
#endif

  black.clear();
  markRoots();

  while (grey.size() > 0) {
    const auto sexpr = grey.front();
    black.emplace(sexpr);
    grey.pop_front();
    trace(sexpr);
  }

  std::apply([&](auto &&...args) { (args.free(black), ...); }, allocators);

  maxHeapSize = getBytesAlloced() * HEAP_GROWTH_FACTOR;
}

void Heap::markRoots() {
  if (vm.getClosure().has_value()) {
    grey.push_back(vm.getClosure().value());
  }

  for (const auto &[sym, sExpr] : vm.getSymTable()) {
    grey.push_back(sym);
    grey.push_back(sExpr);
  }
  for (const auto &sExpr : vm.getStack()) {
    grey.push_back(sExpr);
  }
  for (const auto &sExpr : vm.getCallFrames()) {
    grey.push_back(sExpr.closure);
  }
}

void Heap::mark(const SExpr *sexpr) {
  if (!black.contains(sexpr)) {
    grey.push_back(sexpr);
  }
}

void Heap::trace(const SExpr *sExpr) {
  if (isa<SExprs>(sExpr)) {
    const auto sExprs = cast<SExprs>(sExpr);
    mark(sExprs->first);
    mark(sExprs->rest);
    return;
  }
  if (isa<Prototype>(sExpr)) {
    const auto proto = cast<Prototype>(sExpr);
    for (const auto &consta : proto->code.consts) {
      mark(consta);
    }
    return;
  }
  if (isa<Closure>(sExpr)) {
    const auto closure = cast<Closure>(sExpr);
    mark(closure->proto);
    std::for_each(
        closure->upvalues.cbegin(),
        closure->upvalues.cend(),
        [&](const auto &upvalue) { mark(upvalue->get()); }
    );
    return;
  }
}

Heap::Heap(VM &vm) : vm(vm), enableGC(true), maxHeapSize(INIT_HEAP_SIZE) {}

std::size_t Heap::getBytesAlloced() {
  std::size_t bytesAlloced = 0;
  std::apply(
      [&](auto &&...args) { ((bytesAlloced += args.getBytesAlloced()), ...); },
      allocators
  );
  return bytesAlloced;
}

GCGuard Heap::pauseGC() { return GCGuard(*this); }
