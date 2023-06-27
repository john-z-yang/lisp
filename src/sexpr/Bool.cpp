#include "Bool.hpp"
#include "Cast.cpp"
#include <memory>
#include <string>

using namespace sexpr;

Bool::Bool(const bool val) : Atom(SExpr::Type::BOOL), val(val) {}

Bool Bool::_true(true);
Bool Bool::_false(false);

std::string Bool::toString() const { return (val) ? "#t" : "#f"; }

bool Bool::equals(const SExpr &other) const {
  if (isa<Bool>(other)) {
    return val == cast<Bool>(other).val;
  }
  return false;
}

Bool &Bool::getInstance(const bool val) {
  if (val) {
    return Bool::_true;
  }
  return Bool::_false;
}

bool Bool::toBool(const SExpr &sExpr) {
  if (&sExpr == &_false) {
    return false;
  }
  return true;
}

bool Bool::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::BOOL;
}

std::string Bool::getTypeName() { return "<Boolean>"; }
