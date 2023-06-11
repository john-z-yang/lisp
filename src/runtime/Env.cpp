#include "Env.hpp"
#include "../common/sexpr/SExpr.hpp"
#include <iostream>
#include <memory>
#include <stdexcept>

void Env::def(const SymAtom *sym, const SExpr *val) {
  auto it = symTable.find(sym);
  if (it != symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym->val +
                                "\" is already defined.");
  }
  symTable.insert({sym, val});
}

void Env::set(const SymAtom *sym, const SExpr *val) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym->val + "\" is not defined.");
  }
  symTable[sym] = val;
}

const SExpr *Env::find(const SymAtom *sym) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym->val + "\" is not defined.");
  }
  return it->second;
}

const Env::SymVals &Env::getSymTable() const { return symTable; }

void Env::defMacro(const SymAtom *sym) { macros.insert(sym); }

bool Env::isMacro(const SymAtom *sym) {
  return macros.find(sym) != macros.end();
}
