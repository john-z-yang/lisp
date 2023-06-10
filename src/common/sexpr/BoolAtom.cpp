#include "BoolAtom.hpp"
#include "../cast.cpp"
#include <memory>
#include <string>

bool BoolAtom::toBool(SExpr *sExpr) {
  if (auto boolAtom = dynamic_cast<BoolAtom *>(sExpr)) {
    return boolAtom->val;
  }
  return true;
}

BoolAtom::BoolAtom(const bool val) : Atom(SExpr::Type::BOOL), val(val) {}

BoolAtom::BoolAtom(SExpr *const sExpr)
    : Atom(SExpr::Type::BOOL), val(toBool(sExpr)) {}

std::string BoolAtom::toString() const { return (val) ? "#t" : "#f"; }

bool BoolAtom::equals(const SExpr &other) const {
  if (isa<BoolAtom>(other)) {
    return val == dynamic_cast<const BoolAtom &>(other).val;
  }
  return false;
}

bool BoolAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::BOOL;
}

const std::string BoolAtom::typeName = "<Boolean>";
