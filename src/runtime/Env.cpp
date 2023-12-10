#include "Env.hpp"
#include "../sexpr/NatFn.hpp"
#include "../sexpr/SExpr.hpp"
#include <iostream>
#include <memory>
#include <optional>
#include <stdexcept>

using namespace sexpr;
using namespace runtime;

void Env::regMacro(const Sym *sym) { macros.insert(sym); }

void Env::regBuiltIn(const Sym *sym) { builtInFns.insert(sym); }

void Env::guardMutation(const Sym *sym) {
  if (auto it = macros.find(sym); it != macros.end()) [[unlikely]] {
    throw std::invalid_argument(
        "Cannot mutate macro symbol \"" + sym->val + "\'."
    );
  }
  if (auto it = builtInFns.find(sym); it != builtInFns.end()) [[unlikely]] {
    throw std::invalid_argument(
        "Cannot mutate symbol for built-in function \"" + sym->val + "\'."
    );
  }
}

void Env::def(const Sym *sym, const SExpr *val) {
  guardMutation(sym);
  symTable.insert_or_assign(sym, val);
}

void Env::defMacro(const Sym *sym, const SExpr *macro) {
  def(sym, macro);
  regMacro(sym);
}

void Env::defBuiltIn(const sexpr::Sym *sym, const sexpr::NatFn *builtIn) {
  def(sym, builtIn);
  regBuiltIn(sym);
}

void Env::defBuiltInFns(
    const std::initializer_list<
        std::tuple<const sexpr::Sym *, const sexpr::NatFn *>> natFns
) {
  for (const auto &[sym, natFn] : natFns) {
    defBuiltIn(sym, natFn);
  }
}

void Env::set(const Sym *sym, const SExpr *val) {
  guardMutation(sym);
  auto it = symTable.find(sym);
  if (it == symTable.end()) [[unlikely]] {
    throw std::invalid_argument("Symbol \"" + sym->val + "\" is not defined.");
  }
  it->second = val;
}

const SExpr *Env::load(const Sym *sym) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) [[unlikely]] {
    throw std::invalid_argument("Symbol \"" + sym->val + "\" is not defined.");
  }
  return it->second;
}

bool Env::isMacro(const Sym *sym) { return macros.find(sym) != macros.end(); }

bool Env::isNatFn(const sexpr::Sym *sym) {
  return builtInFns.find(sym) != builtInFns.end();
}

const std::unordered_map<const sexpr::Sym *, const sexpr::SExpr *> &
Env::getSymTable() const {
  return symTable;
}
