#include "../../include/sexpr/SymAtom.hpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

SymAtom::SymAtom(string val) : Atom(SExpr::Type::SYM), val(val) {}

string SymAtom::toString() const { return val; }

bool SymAtom::equals(const SExpr &other) const {
  if (other.type != SExpr::SYM) {
    return false;
  }
  return val == dynamic_cast<const SymAtom &>(other).val;
}

bool SymAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::SYM;
}