#include "Env.hpp"
#include "../common/sexpr/SExpr.hpp"
#include <iostream>
#include <memory>
#include <stdexcept>

void Env::def(SymAtom *sym, SExpr *val) {
  auto it = symTable.find(sym);
  if (it != symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym->val +
                                "\" is already defined.");
  }
  symTable.insert({sym, val});
}

void Env::set(SymAtom *sym, SExpr *val) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym->val + "\" is not defined.");
  }
  symTable[sym] = val;
}

SExpr *Env::find(SymAtom *sym) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym->val + "\" is not defined.");
  }
  return it->second;
}

const Env::SymVals &Env::getSymTable() const { return symTable; }

void Env::defMacro(SymAtom *sym) { macros.insert(sym); }

bool Env::isMacro(SymAtom *sym) { return macros.find(sym) != macros.end(); }
