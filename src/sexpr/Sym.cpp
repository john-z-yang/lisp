#include "Sym.hpp"
#include "Cast.cpp"
#include <memory>
#include <string>

using namespace sexpr;

std::ostream &Sym::serialize(std::ostream &o) const { return o << val; }

bool Sym::equals(const SExpr &other) const {
  if (isa<Sym>(other)) {
    return val == cast<Sym>(other).val;
  }
  return false;
}

size_t Sym::HashFunction::operator()(const Sym &sym) const { return sym.hash; }

bool Sym::EqualFunction::operator()(const Sym &lhs, const Sym &rhs) const {
  return lhs.hash == rhs.hash;
}

Sym::Sym(const ValueType val)
    : Atom(SExpr::Type::SYM), val(val), hash(std::hash<ValueType>()(val)) {}

bool Sym::classOf(const SExpr &sExpr) { return sExpr.type == SExpr::Type::SYM; }

std::string Sym::getTypeName() { return "<Symbol>"; }
