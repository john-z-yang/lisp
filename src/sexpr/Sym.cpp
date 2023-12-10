#include "Sym.hpp"
#include "Casting.hpp"
#include <memory>
#include <string>

using namespace sexpr;

std::ostream &Sym::serialize(std::ostream &o) const { return o << val; }

bool Sym::equals(const SExpr &other) const {
  if (const auto sym = dynCast<Sym>(other)) {
    return val == sym->get().val;
  }
  return false;
}

Sym::Sym(const ValueType val) : Atom(SExpr::Type::SYM), val(val) {}

bool Sym::classOf(const SExpr *sExpr) {
  return sExpr->type == SExpr::Type::SYM;
}

std::string Sym::getTypeName() { return "<Symbol>"; }
