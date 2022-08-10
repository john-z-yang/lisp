#include "../../include/sexpr/IntAtom.hpp"
#include <string>

using std::string;
using std::to_string;

IntAtom::IntAtom(const int val) : Atom(SExpr::Type::NUM), val(val) {}

string IntAtom::toString() const { return to_string(val); }

bool IntAtom::equals(const SExpr &other) const {
  if (other.type != SExpr::Type::NUM) {
    return false;
  }
  return val == dynamic_cast<const IntAtom &>(other).val;
}