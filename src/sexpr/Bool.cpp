#include "Bool.hpp"
#include "cast.cpp"
#include <memory>
#include <string>

using namespace sexpr;

Bool::Bool(const bool val) : Atom(SExpr::Type::BOOL), val(val) {}

std::string Bool::toString() const { return (val) ? "#t" : "#f"; }

bool Bool::equals(const SExpr &other) const {
  if (isa<Bool>(other)) {
    return val == cast<Bool>(other).val;
  }
  return false;
}

Bool *Bool::getInstance(const bool val) {
  if (val) {
    return &Bool::_true;
  }
  return &Bool::_false;
}

bool Bool::toBool(const SExpr *sExpr) {
  if (sExpr == &_false) {
    return false;
  }
  return true;
}

bool Bool::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::BOOL;
}

Bool Bool::_true(true);
Bool Bool::_false(false);

const std::string Bool::typeName = "<Boolean>";