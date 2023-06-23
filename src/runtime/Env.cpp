#include "Env.hpp"
#include "../sexpr/SExpr.hpp"
#include <iostream>
#include <memory>
#include <stdexcept>

using namespace runtime;
using namespace sexpr;

void Env::def(const Sym *sym, const SExpr *val) {
  auto it = symTable.find(sym);
  if (it != symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym->val +
                                "\" is already defined.");
  }
  symTable.insert({sym, val});
}

void Env::set(const Sym *sym, const SExpr *val) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym->val + "\" is not defined.");
  }
  symTable[sym] = val;
}

const SExpr *Env::find(const Sym *sym) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw std::invalid_argument("Symbol \"" + sym->val + "\" is not defined.");
  }
  return it->second;
}

const Env::SymTable &Env::getSymTable() const { return symTable; }

void Env::defMacro(const Sym *sym) { macros.insert(sym); }

bool Env::isMacro(const Sym *sym) { return macros.find(sym) != macros.end(); }
