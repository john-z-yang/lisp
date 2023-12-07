#include "Heap.hpp"
#include "../sexpr/Cast.cpp"
#include "../sexpr/SExprs.hpp"
#include "CallFrame.hpp"
#include "Env.hpp"
#include "GCGuard.hpp"
#include <algorithm>

using namespace sexpr;
using namespace runtime;

void Heap::gc() {
  if (!enableGC) {
    return;
  }
#ifndef GC_STRESS_TEST
  if (heap.size() < gcHeapSize) {
    return;
  }
#endif

  black.clear();

  grey.push_back(closure.value());
  markGlobals();
  markStack();
  markCallFrames();
  markOpenUpvalues();

  while (grey.size() > 0) {
    const auto sexpr = grey.front();
    black.emplace(sexpr);
    grey.pop_front();
    trace(sexpr);
  }

  std::erase_if(heap, [&](const auto &unique) {
    return !black.contains(unique.get());
  });

  gcHeapSize = heap.size() * FREESTORE_HEAP_GROWTH_FACTOR;
}

void Heap::mark(const SExpr *sexpr) {
  if (!black.contains(sexpr)) {
    grey.push_back(sexpr);
  }
}

void Heap::trace(const SExpr *sexpr) {
  if (isa<SExprs>(sexpr)) {
    const auto sexprs = cast<SExprs>(sexpr);
    mark(sexprs->first);
    mark(sexprs->rest);
    return;
  }
  if (isa<Prototype>(sexpr)) {
    const auto fnAtom = cast<Prototype>(sexpr);
    std::for_each(
        fnAtom->code.consts.cbegin(),
        fnAtom->code.consts.cend(),
        [&](const auto &sexpr) { mark(sexpr); }
    );
    return;
  }
  if (isa<Closure>(sexpr)) {
    const auto closureAtom = cast<Closure>(sexpr);
    mark(closureAtom->proto);
    std::for_each(
        closureAtom->upvalues.cbegin(),
        closureAtom->upvalues.cend(),
        [&](const auto &upvalue) { mark(upvalue->get()); }
    );
    return;
  }
}

void Heap::markGlobals() {
  std::for_each(
      globals.getSymTable().cbegin(),
      globals.getSymTable().cend(),
      [&](const auto &it) {
        const auto &[sym, sexpr] = it;
        grey.push_back(sym);
        grey.push_back(sexpr);
      }
  );
}

void Heap::markStack() {
  std::transform(
      stack.cbegin(),
      stack.cend(),
      std::back_inserter(grey),
      [](const auto &sexpr) { return sexpr; }
  );
}

void Heap::markCallFrames() {
  std::transform(
      callFrames.cbegin(),
      callFrames.cend(),
      std::back_inserter(grey),
      [](const auto &callFrame) { return callFrame.closure; }
  );
}

void Heap::markOpenUpvalues() {
  std::transform(
      openUpvals.cbegin(),
      openUpvals.cend(),
      std::back_inserter(grey),
      [](const auto &it) { return it.second->get(); }
  );
}

Heap::Heap(
    Env &globals,
    std::optional<const sexpr::Closure *> &closure,
    std::vector<const sexpr::SExpr *> &stack,
    std::vector<CallFrame> &callFrames,
    std::unordered_map<StackPtr, std::shared_ptr<Upvalue>> &openUpvals
)
    : globals(globals),
      closure(closure),
      stack(stack),
      callFrames(callFrames),
      openUpvals(openUpvals),
      enableGC(false),
      gcHeapSize(FREESTORE_INIT_HEAP_SIZE) {
  for (Num::ValueType i{FREESTORE_INT_CACHE_MIN}; i <= FREESTORE_INT_CACHE_MAX;
       i++) {
    numCache.push_back(std::make_unique<Num>(i));
  }
}

GCGuard Heap::startGC() { return GCGuard(*this); }

GCGuard Heap::pauseGC() { return GCGuard(*this); }
