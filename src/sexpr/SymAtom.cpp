#include "SymAtom.hpp"
#include "cast.cpp"
#include <memory>
#include <string>

SymAtom::SymAtom(std::string val) : Atom(SExpr::Type::SYM), val(val) {}

std::string SymAtom::toString() const { return val; }

bool SymAtom::equals(const SExpr &other) const {
  if (isa<SymAtom>(other)) {
    return val == dynamic_cast<const SymAtom &>(other).val;
  }
  return false;
}

bool SymAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::SYM;
}

const std::string SymAtom::typeName = "Symbol";

size_t SymAtom::HashFunction::operator()(const SymAtom &sym) const {
  return std::hash<std::string>()(sym.val);
}