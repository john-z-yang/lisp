#include "Env.hpp"
#include "../sexpr/NatFn.hpp"
#include "../sexpr/SExpr.hpp"
#include <iostream>
#include <memory>
#include <optional>
#include <stdexcept>

using namespace sexpr;
using namespace runtime;

void Env::regMacro(const Sym &sym) { macros.insert(sym); }

void Env::regNative(const Sym &sym) { natFns.insert(sym); }

void Env::guardMutation(const Sym &sym) {
  if (auto it = macros.find(sym); it != macros.end()) [[unlikely]] {
    throw std::invalid_argument(
        "Cannot mutate macro symbol \"" + sym.val + "\'."
    );
  }
  if (auto it = natFns.find(sym); it != natFns.end()) [[unlikely]] {
    throw std::invalid_argument(
        "Cannot mutate native symbol \"" + sym.val + "\'."
    );
  }
}

void Env::def(const Sym &sym, const SExpr &val) {
  guardMutation(sym);
  symTable.insert_or_assign(sym, val);
}

void Env::defMacro(const Sym &sym, const SExpr &macro) {
  def(sym, macro);
  regMacro(sym);
}

void Env::defNatFns(
    const std::initializer_list<
        std::tuple<const sexpr::Sym &, const sexpr::NatFn &>> natives
) {
  for (const auto &[sym, natFn] : natives) {
    def(sym, natFn);
    regNative(sym);
  }
}

void Env::set(const Sym &sym, const SExpr &val) {
  guardMutation(sym);
  auto it = symTable.find(sym);
  if (it == symTable.end()) [[unlikely]] {
    throw std::invalid_argument("Symbol \"" + sym.val + "\" is not defined.");
  }
  it->second = val;
}

const SExpr &Env::load(const Sym &sym) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) [[unlikely]] {
    throw std::invalid_argument("Symbol \"" + sym.val + "\" is not defined.");
  }
  return it->second;
}

std::optional<const std::reference_wrapper<const sexpr::SExpr>>
Env::find(const sexpr::Sym &sym) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    return std::nullopt;
  }
  return it->second;
}

bool Env::isMacro(const Sym &sym) { return macros.find(sym) != macros.end(); }

bool Env::isNatFn(const sexpr::Sym &sym) {
  return natFns.find(sym) != natFns.end();
}

const Env::SymTable &Env::getSymTable() const { return symTable; }
