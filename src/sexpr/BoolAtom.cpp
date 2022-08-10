#include "../../include/sexpr/BoolAtom.hpp"
#include <memory>
#include <string>

using std::dynamic_pointer_cast;
using std::shared_ptr;
using std::string;

bool BoolAtom::cast(const shared_ptr<SExpr> sExpr) {
  if (sExpr->type == SExpr::Type::BOOL) {
    return dynamic_pointer_cast<BoolAtom>(sExpr)->val;
  }
  return true;
}

BoolAtom::BoolAtom(const bool val) : Atom(SExpr::Type::BOOL), val(val) {}

BoolAtom::BoolAtom(const shared_ptr<SExpr> sExpr)
    : Atom(SExpr::Type::BOOL), val(cast(sExpr)) {}

string BoolAtom::toString() const { return (val) ? "#t" : "#f"; }

bool BoolAtom::equals(const SExpr &other) const {
  if (other.type != SExpr::Type::BOOL) {
    return false;
  }
  return val == dynamic_cast<const BoolAtom &>(other).val;
}