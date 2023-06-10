#include "BoolAtom.hpp"
#include "../cast.cpp"
#include <memory>
#include <string>

BoolAtom::BoolAtom(const bool val) : Atom(SExpr::Type::BOOL), val(val) {}

std::string BoolAtom::toString() const { return (val) ? "#t" : "#f"; }

bool BoolAtom::equals(const SExpr &other) const {
  if (isa<BoolAtom>(other)) {
    return val == dynamic_cast<const BoolAtom &>(other).val;
  }
  return false;
}

BoolAtom *BoolAtom::getInstance(const bool val) {
  if (val) {
    return &BoolAtom::_true;
  }
  return &BoolAtom::_false;
}

bool BoolAtom::toBool(SExpr *sExpr) {
  if (auto boolAtom = dynamic_cast<BoolAtom *>(sExpr)) {
    return boolAtom->val;
  }
  return true;
}

bool BoolAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::BOOL;
}

BoolAtom BoolAtom::_true(true);
BoolAtom BoolAtom::_false(false);

const std::string BoolAtom::typeName = "<Boolean>";
