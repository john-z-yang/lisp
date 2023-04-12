#include "Env.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/cast.cpp"
#include "../vm/RuntimeException.hpp"
#include <iostream>
#include <memory>

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