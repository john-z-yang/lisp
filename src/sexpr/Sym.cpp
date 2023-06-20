#include "Sym.hpp"
#include "cast.cpp"
#include <memory>
#include <string>

using namespace sexpr;

Sym::Sym(std::string val)
    : Atom(SExpr::Type::SYM), val(val), hash(std::hash<std::string>()(val)) {}

std::string Sym::toString() const { return val; }

bool Sym::equals(const SExpr &other) const {
  if (isa<Sym>(other)) {
    return val == cast<Sym>(other).val;
  }
  return false;
}

bool Sym::classOf(const SExpr &sExpr) { return sExpr.type == SExpr::Type::SYM; }

const std::string Sym::typeName = "<Symbol>";

size_t Sym::HashFunction::operator()(const Sym *sym) const { return sym->hash; }

bool Sym::EqualFunction::operator()(const Sym *lhs, const Sym *rhs) const {
  return lhs->hash == rhs->hash;
}
