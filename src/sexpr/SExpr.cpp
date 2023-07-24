#include "SExpr.hpp"
#include <iostream>

using namespace sexpr;

SExpr::SExpr(SExpr::Type type) : type(type) {}

SExpr::~SExpr() {}

std::ostream &sexpr::operator<<(std::ostream &o, const SExpr &sExpr) {
  return sExpr.serialize(o);
}

bool sexpr::operator==(const SExpr &lhs, const SExpr &rhs) {
  return lhs.equals(rhs);
}
