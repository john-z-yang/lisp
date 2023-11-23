#include "Bool.hpp"
#include "Cast.cpp"
#include <memory>
#include <string>

using namespace sexpr;

Bool::Bool(Bool::ValueType val) : Atom(SExpr::Type::BOOL), val(val) {}

Bool Bool::_true(true);
Bool Bool::_false(false);

std::ostream &Bool::serialize(std::ostream &o) const {
  return (val) ? o << "#t" : o << "#f";
}

bool Bool::equals(const SExpr &other) const {
  if (isa<Bool>(other)) {
    return val == cast<Bool>(other).val;
  }
  return false;
}

void Bool::fixupAddrs(const runtime::BreakTable &) {}

Bool *Bool::getInstance(const Bool::ValueType val) {
  if (val) {
    return &Bool::_true;
  }
  return &Bool::_false;
}

bool Bool::toBool(SExpr *sExpr) { return sExpr != &_false; }

bool Bool::classOf(const SExpr *sExpr) {
  return sExpr->type == SExpr::Type::BOOL;
}

std::string Bool::getTypeName() { return "<Boolean>"; }
