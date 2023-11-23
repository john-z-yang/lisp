#include "Heap.hpp"
#include "../sexpr/Cast.cpp"
#include "../sexpr/SExprs.hpp"
#include "BreakTable.hpp"
#include "GCGuard.hpp"
#include "VM.hpp"
#include <algorithm>
#include <functional>
#include <unordered_map>

using namespace sexpr;
using namespace runtime;

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
  for (const auto &callFrame : vm.getCallFrames()) {
    grey.push_back(callFrame.closure);
  }
  for (const auto &[_, upvalue] : vm.getOpenUpvals()) {
    grey.push_back(upvalue);
  }
}

void Heap::markSExpr(SExpr *sExpr) {
  if (!black.contains(sExpr)) {
    grey.push_back(sExpr);
  }
}

void Heap::trace(SExpr *sExpr) {
  if (isa<SExprs>(sExpr)) {
    const auto sExprs = cast<SExprs>(sExpr);
    markSExpr(sExprs->first);
    markSExpr(sExprs->rest);
    return;
  }
  if (isa<Prototype>(sExpr)) {
    const auto proto = cast<Prototype>(sExpr);
    for (const auto &consta : proto->code.consts) {
      markSExpr(consta);
    }
    return;
  }
  if (isa<Closure>(sExpr)) {
    const auto closureAtom = cast<Closure>(sExpr);
    markSExpr(closureAtom->proto);
    for (const auto &upvalue : closureAtom->upvalues) {
      markSExpr(upvalue);
    }
    return;
  }
  if (isa<Upvalue>(sExpr)) {
    const auto upvalue = cast<Upvalue>(sExpr);
    markSExpr(upvalue->get());
  }
}

Heap::Heap(VM &vm) : vm(vm), enableGC(true), maxHeapSize(INIT_MAX_HEAP_SIZE) {}

const BreakTable Heap::gc() {
  if (!enableGC) {
    return {};
  }
#ifndef GC_STRESS_TEST
  if (getBytesAlloced() < maxHeapSize) {
    return {};
  }
#endif

  black.clear();
  markRoots();

  while (grey.size() > 0) {
    const auto sExpr = grey.front();
    black.emplace(sExpr);
    grey.pop_front();
    trace(sExpr);
  }

  BreakTable breakTable;
  free(black, breakTable);

  vm.fixupAddrs(breakTable);

  maxHeapSize = getBytesAlloced() * HEAP_GROWTH_FACTOR;
  return breakTable;
}

GCGuard Heap::pauseGC() { return GCGuard(*this); }

std::size_t Heap::getBytesAlloced() {
  std::size_t bytesAlloced = 0;
  std::apply(
      [&](auto &&...args) { ((bytesAlloced += args.getBytesAlloced()), ...); },
      allocators
  );
  return bytesAlloced;
}

void Heap::free(
    const std::unordered_set<sexpr::SExpr *> &reachable, BreakTable &breakTable
) {
  std::apply(
      [&](auto &&...args) { (args.free(reachable, breakTable), ...); },
      allocators
  );
  std::apply(
      [&](auto &&...args) { (args.fixupAddrs(breakTable), ...); }, allocators
  );
}
