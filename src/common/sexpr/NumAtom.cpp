#include "NumAtom.hpp"
#include "../cast.cpp"
#include <sstream>
#include <string>

NumAtom::NumAtom(const double val) : Atom(SExpr::Type::NUM), val(val) {}

std::string NumAtom::toString() const {
  std::stringstream ss;
  ss << val;
  return ss.str();
}

bool NumAtom::equals(const SExpr &other) const {
  if (isa<NumAtom>(other)) {
    return val == cast<NumAtom>(other).val;
  }
  return false;
}

bool NumAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::NUM;
}

const std::string NumAtom::typeName = "<Number>";
