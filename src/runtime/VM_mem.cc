#include "../sexpr/SExprs.hpp"
#include "../sexpr/cast.cpp"
#include "VM.hpp"

using namespace sexpr;
using namespace runtime;

void VM::gc() {
  black.clear();

  markGlobals();
  markStack();
  markCallFrames();
  markOpenUpvalues();

  while (grey.size() > 0) {
    const auto sexpr = *grey.begin();
    black.emplace(sexpr);
    grey.erase(grey.begin());
    trace(sexpr);
  }
  std::erase_if(
      heap, [&](const auto &unique) { return !black.contains(unique.get()); });
}

void VM::mark(const SExpr *sexpr) {
  if (!black.contains(sexpr)) {
    grey.emplace(sexpr);
  }
}

void VM::trace(const SExpr *sexpr) {
  if (const auto sexprs = dynCast<SExprs>(sexpr)) {
    mark(sexprs->first);
    mark(sexprs->rest);
    return;
  }
  if (const auto fnAtom = dynCast<Fn>(sexpr)) {
    std::for_each(fnAtom->code.consts.begin(), fnAtom->code.consts.end(),
                  [&](const SExpr *sexpr) { mark(sexpr); });
    return;
  }
  if (const auto closureAtom = dynCast<Closure>(sexpr)) {
    mark(closureAtom->fnAtom);
    std::for_each(
        closureAtom->upvalues.begin(), closureAtom->upvalues.end(),
        [&](const std::shared_ptr<Upvalue> upvalue) { mark(upvalue->get()); });
    return;
  }
}

void VM::markGlobals() {
  for (const auto &[sym, sexpr] : globals.getSymTable()) {
    grey.emplace(sym);
    grey.emplace(sexpr);
  }
}
void VM::markStack() {
  for (const auto &sexpr : stack) {
    grey.emplace(sexpr);
  }
}
void VM::markCallFrames() {
  for (const auto &callFrame : callFrames) {
    grey.emplace(callFrame.closure);
  }
}
void VM::markOpenUpvalues() {
  for (const auto &[_, openUpvalue] : openUpvalues) {
    grey.emplace(openUpvalue->get());
  }
}
