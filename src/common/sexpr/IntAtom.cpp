#include "IntAtom.hpp"
#include "../cast.cpp"
#include <string>

IntAtom::IntAtom(const int val) : Atom(SExpr::Type::NUM), val(val) {}

std::string IntAtom::toString() const { return std::to_string(val); }

bool IntAtom::equals(const SExpr &other) const {
  if (isa<IntAtom>(other)) {
    return val == dynamic_cast<const IntAtom &>(other).val;
  }
  return false;
}

bool IntAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::NUM;
}

const std::string IntAtom::typeName = "Integer";
