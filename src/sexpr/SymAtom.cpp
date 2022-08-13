#include "SymAtom.hpp"
#include "cast.cpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

SymAtom::SymAtom(string val) : Atom(SExpr::Type::SYM), val(val) {}

string SymAtom::toString() const { return val; }

bool SymAtom::equals(const SExpr &other) const {
  if (isa<SymAtom>(other)) {
    return val == dynamic_cast<const SymAtom &>(other).val;
  }
  return false;
}

bool SymAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::SYM;
}

const string SymAtom::typeName = "Symbol";