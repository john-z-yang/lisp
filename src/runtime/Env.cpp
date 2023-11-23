#include "Env.hpp"
#include "../sexpr/Cast.cpp"
#include "../sexpr/NatFn.hpp"
#include "../sexpr/SExpr.hpp"
#include <algorithm>
#include <iostream>
#include <memory>
#include <optional>
#include <stdexcept>
#include <utility>

using namespace sexpr;
using namespace runtime;

void Env::regMacro(Sym *sym) { macros.insert(sym); }

void Env::regNative(Sym *sym) { natFns.insert(sym); }

void Env::guardMutation(Sym *sym) {
  if (auto it = macros.find(sym); it != macros.end()) [[unlikely]] {
    throw std::invalid_argument(
        "Cannot mutate macro symbol \"" + sym->val + "\'."
    );
  }
  if (auto it = natFns.find(sym); it != natFns.end()) [[unlikely]] {
    throw std::invalid_argument(
        "Cannot mutate native symbol \"" + sym->val + "\'."
    );
  }
}

void Env::def(Sym *sym, SExpr *val) {
  guardMutation(sym);
  symTable.insert_or_assign(sym, val);
}

void Env::defMacro(Sym *sym, SExpr *macro) {
  def(sym, macro);
  regMacro(sym);
}

void Env::defNatFn(sexpr::Sym *sym, sexpr::NatFn *natFn) {
  def(sym, natFn);
  regNative(sym);
}

void Env::defNatFns(
    const std::initializer_list<std::tuple<sexpr::Sym *, sexpr::NatFn *>> natFns
) {
  for (const auto &[sym, natFn] : natFns) {
    defNatFn(sym, natFn);
  }
}

void Env::set(Sym *sym, SExpr *val) {
  guardMutation(sym);
  auto it = symTable.find(sym);
  if (it == symTable.end()) [[unlikely]] {
    throw std::invalid_argument("Symbol \"" + sym->val + "\" is not defined.");
  }
  it->second = val;
}

SExpr *Env::load(Sym *sym) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) [[unlikely]] {
    throw std::invalid_argument("Symbol \"" + sym->val + "\" is not defined.");
  }
  return it->second;
}

bool Env::isMacro(Sym *sym) { return macros.find(sym) != macros.end(); }

bool Env::isNatFn(sexpr::Sym *sym) { return natFns.find(sym) != natFns.end(); }

const Env::SymTable &Env::getSymTable() const { return symTable; }

void Env::fixupAddrs(const BreakTable &breakTable) {
  SymTable updatedSymTable;
  for (const auto &[sym, sExpr] : symTable) {
    updatedSymTable.insert(
        std::make_pair(cast<Sym>(breakTable.get(sym)), breakTable.get(sExpr))
    );
  }

  symTable.clear();
  symTable = updatedSymTable;

  SymSet updatedMacros;
  for (auto &sym : macros) {
    updatedMacros.emplace(cast<Sym>(breakTable.get(sym)));
  }
  macros.clear();
  macros = updatedMacros;

  SymSet updatedNatfns;
  for (auto &sym : natFns) {
    updatedNatfns.emplace(cast<Sym>(breakTable.get(sym)));
  }
  natFns.clear();
  natFns = updatedNatfns;
}
