#include "SExpr.hpp"
#include "SExprs.hpp"
#include "cast.cpp"
#include <iostream>

using namespace sexpr;

SExpr::SExpr(SExpr::Type type) : type(type) {}

SExpr::~SExpr() {}

std::ostream &sexpr::operator<<(std::ostream &o, const SExpr &sExpr) {
  return o << (isa<SExprs>(sExpr) ? "(" : "") << sExpr.toString();
}

bool sexpr::operator==(const SExpr &lhs, const SExpr &rhs) {
  return lhs.equals(rhs);
}
