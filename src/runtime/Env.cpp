#include "Env.hpp"
#include "../common/sexpr/NatFnAtom.hpp"
#include "../common/sexpr/SExpr.hpp"
#include "NatFnImpls.hpp"
#include <iostream>
#include <memory>
#include <stdexcept>

Env::Env() {
#define BIND_NATIVE_FN(sym, func, argc)                                        \
  do {                                                                         \
    symTable.insert({SymAtom(sym), std::make_shared<NatFnAtom>(&func, argc)}); \
  } while (false)

  BIND_NATIVE_FN("sym?", lispIsSym, 1);
  BIND_NATIVE_FN("gensym", lispGenSym, 0);

  BIND_NATIVE_FN("num?", lispIsNum, 1);
  BIND_NATIVE_FN("=", lispNumEq, -1);
  BIND_NATIVE_FN(">", lispGt, -1);
  BIND_NATIVE_FN(">=", lispGteq, -1);
  BIND_NATIVE_FN("<", lispLt, -1);
  BIND_NATIVE_FN("<=", lispLteq, -1);
  BIND_NATIVE_FN("+", lispAdd, -1);
  BIND_NATIVE_FN("*", lispMult, -1);
  BIND_NATIVE_FN("-", lispSub, -1);
  BIND_NATIVE_FN("/", lispDiv, -1);
  BIND_NATIVE_FN("abs", lispAbs, 1);
  BIND_NATIVE_FN("%", lispMod, 2);

  BIND_NATIVE_FN("str?", lispIsStr, 1);
  BIND_NATIVE_FN("str-len", lispStrLen, 1);
  BIND_NATIVE_FN("str-sub", lispStrSub, 3);
  BIND_NATIVE_FN("str-con", lispStrCon, -1);
  BIND_NATIVE_FN("->str", lispToStr, 1);

  BIND_NATIVE_FN("null?", lispIsNull, 1);
  BIND_NATIVE_FN("cons?", lispIsCons, 1);
  BIND_NATIVE_FN("cons", lispCons, 2);
  BIND_NATIVE_FN("car", lispCar, 1);
  BIND_NATIVE_FN("cdr", lispCdr, 1);

  BIND_NATIVE_FN("dis", lispDis, 1);
  BIND_NATIVE_FN("display", lispDisplay, 1);

  BIND_NATIVE_FN("quit", lispQuit, 0);
  BIND_NATIVE_FN("error", lispError, 1);

  BIND_NATIVE_FN("eq?", lispEq, 2);
  BIND_NATIVE_FN("eqv?", lispEqv, 2);

#undef BIND_NATIVE_FN
}

void Env::def(SymAtom &sym, std::shared_ptr<SExpr> val) {
  auto it = symTable.find(sym);
  if (it != symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym.val +
                                "\" is already defined.");
  }
  symTable.insert({sym, val});
}

void Env::set(SymAtom &sym, std::shared_ptr<SExpr> val) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym.val + "\" is not defined.");
  }
  symTable[sym] = val;
}

std::shared_ptr<SExpr> Env::find(SymAtom &sym) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym.val + "\" is not defined.");
  }
  return it->second;
}

const Env::SymVals &Env::getSymTable() const { return symTable; }

void Env::defMacro(SymAtom &sym) { macros.insert(sym); }

bool Env::isMacro(SymAtom &sym) { return macros.find(sym) != macros.end(); }
