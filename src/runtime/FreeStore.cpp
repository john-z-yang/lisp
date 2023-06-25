#include "FreeStore.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/cast.cpp"
#include "CallFrame.hpp"
#include "Env.hpp"
#include "GCGuard.hpp"

using namespace sexpr;
using namespace runtime;

void FreeStore::gc() {
  if (!enableGC || heap.size() < gcHeapSize) {
    return;
  }
  black.clear();

  markGlobals();
  markStack();
  markCallFrames();
  markOpenUpvalues();

  while (grey.size() > 0) {
    const auto sexpr = grey.front();
    black.emplace(sexpr);
    grey.pop_front();
    trace(*sexpr);
  }

  std::erase_if(
      heap, [&](const auto &unique) { return !black.contains(unique.get()); });

  gcHeapSize = heap.size() * FREESTORE_HEAP_GROWTH_FACTOR;
}

void FreeStore::mark(const SExpr &sexpr) {
  if (!black.contains(&sexpr)) {
    grey.push_back(&sexpr);
  }
}

void FreeStore::trace(const SExpr &sexpr) {
  if (isa<SExprs>(sexpr)) {
    const auto &sexprs = cast<SExprs>(sexpr);
    mark(sexprs.first);
    mark(sexprs.rest);
    return;
  }
  if (isa<Fn>(sexpr)) {
    const auto &fnAtom = cast<Fn>(sexpr);
    std::for_each(fnAtom.code.consts.begin(), fnAtom.code.consts.end(),
                  [&](const auto &sexpr) { mark(sexpr); });
    return;
  }
  if (isa<Closure>(sexpr)) {
    const auto &closureAtom = cast<Closure>(sexpr);
    mark(closureAtom.fnAtom);
    std::for_each(closureAtom.upvalues.begin(), closureAtom.upvalues.end(),
                  [&](const auto &upvalue) { mark(upvalue->get()); });
    return;
  }
}

void FreeStore::markGlobals() {
  for (const auto &[sym, sexpr] : globals.getSymTable()) {
    grey.push_back(&sym.get());
    grey.push_back(&sexpr.get());
  }
}

void FreeStore::markStack() {
  for (const auto &sexpr : stack) {
    grey.push_back(&sexpr.get());
  }
}

void FreeStore::markCallFrames() {
  for (const auto &callFrame : callFrames) {
    grey.push_back(&callFrame.closure);
  }
}

void FreeStore::markOpenUpvalues() {
  for (const auto &[_, openUpvalue] : openUpvals) {
    grey.push_back(&openUpvalue->get());
  }
}

FreeStore::FreeStore(
    Env &globals,
    std::vector<std::reference_wrapper<const sexpr::SExpr>> &stack,
    std::vector<CallFrame> &callFrames,
    std::unordered_map<StackPtr, std::shared_ptr<Upvalue>> &openUpvals)
    : globals(globals), stack(stack), callFrames(callFrames),
      openUpvals(openUpvals), enableGC(false),
      gcHeapSize(FREESTORE_INIT_HEAP_SIZE) {
  for (double i{FREESTORE_INT_CACHE_MIN}; i <= FREESTORE_INT_CACHE_MAX; i++) {
    intCache.push_back(std::make_unique<Num>(i));
  }
}

GCGuard FreeStore::startGC() { return GCGuard(enableGC, true); }

GCGuard FreeStore::pauseGC() { return GCGuard(enableGC, false); }
