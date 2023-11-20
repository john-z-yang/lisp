#include "SExpr.hpp"
#include <iostream>

using namespace sexpr;

SExpr::SExpr(SExpr::Type type) : type(type) {}

SExpr::~SExpr() {}

bool SExpr::classOf([[maybe_unused]] const SExpr *sExpr) { return true; }

std::string SExpr::getTypeName() { return "<S-expression>"; }

std::ostream &sexpr::operator<<(std::ostream &o, const SExpr &sExpr) {
  return sExpr.serialize(o);
}

bool sexpr::operator==(const SExpr &lhs, const SExpr &rhs) {
  return lhs.equals(rhs);
}
