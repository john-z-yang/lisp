#include "Env.hpp"
#include "../sexpr/NatFnAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/cast.cpp"
#include "../vm/RuntimeException.hpp"
#include <iostream>
#include <memory>

Env::Env() {
#define BIND_NATIVE_FN(sym, func, argc)                                        \
  do {                                                                         \
    symTable.insert(                                                           \
        {SymAtom(#sym), std::make_shared<NatFnAtom>(&func, argc)});            \
  } while (false)

  BIND_NATIVE_FN(display, lispDisplay, 1);

  BIND_NATIVE_FN(=, lispEq, -1);
  BIND_NATIVE_FN(+, lispAdd, -1);
  BIND_NATIVE_FN(>, lispGt, -1);
  BIND_NATIVE_FN(>=, lispGteq, -1);
  BIND_NATIVE_FN(<, lispLt, -1);
  BIND_NATIVE_FN(<=, lispLteq, -1);

  BIND_NATIVE_FN(+, lispAdd, -1);
  BIND_NATIVE_FN(-, lispSub, -1);
  BIND_NATIVE_FN(*, lispMult, -1);
  BIND_NATIVE_FN(/, lispDiv, -1);

  BIND_NATIVE_FN(abs, lispAbs, 1);
  BIND_NATIVE_FN(%, lispMod, 2);

#undef BIND_NATIVE_FN
}

void Env::def(SymAtom &sym, std::shared_ptr<SExpr> val) {
  auto it = symTable.find(sym);
  if (it != symTable.end()) {
    throw RuntimeException("Symbol \"" + sym.val + "\" is already defined.");
  }
  symTable.insert({sym, val});
}

void Env::set(SymAtom &sym, std::shared_ptr<SExpr> val) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw RuntimeException("Symbol \"" + sym.val + "\" is not defined.");
  }
  symTable[sym] = val;
}

std::shared_ptr<SExpr> Env::find(SymAtom &sym) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw RuntimeException("Symbol \"" + sym.val + "\" is not defined.");
  }
  return it->second;
}