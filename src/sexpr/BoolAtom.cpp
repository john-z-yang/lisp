#include "../../include/sexpr/BoolAtom.hpp"
#include "cast.cpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

bool BoolAtom::toBool(shared_ptr<SExpr> sExpr) {
  if (isa<BoolAtom>(*sExpr)) {
    return cast<BoolAtom>(sExpr)->val;
  }
  return true;
}

BoolAtom::BoolAtom(const bool val) : Atom(SExpr::Type::BOOL), val(val) {}

BoolAtom::BoolAtom(const shared_ptr<SExpr> sExpr)
    : Atom(SExpr::Type::BOOL), val(toBool(sExpr)) {}

string BoolAtom::toString() const { return (val) ? "#t" : "#f"; }

bool BoolAtom::equals(const SExpr &other) const {
  if (isa<BoolAtom>(other)) {
    return val == dynamic_cast<const BoolAtom &>(other).val;
  }
  return false;
}

bool BoolAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::BOOL;
}