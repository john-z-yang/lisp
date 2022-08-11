#include "../../include/sexpr/IntAtom.hpp"
#include "cast.cpp"
#include <string>

using std::string;
using std::to_string;

IntAtom::IntAtom(const int val) : Atom(SExpr::Type::NUM), val(val) {}

string IntAtom::toString() const { return to_string(val); }

bool IntAtom::equals(const SExpr &other) const {
  if (isa<IntAtom>(other)) {
    return val == dynamic_cast<const IntAtom &>(other).val;
  }
  return false;
}

bool IntAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::NUM;
}