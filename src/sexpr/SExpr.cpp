#include "SExpr.hpp"
#include "SExprs.hpp"
#include "cast.cpp"
#include <iostream>

SExpr::SExpr(SExpr::Type type) : type(type) {}

std::ostream &operator<<(std::ostream &o, const SExpr &sExpr) {
  return o << (isa<SExprs>(sExpr) ? "(" : "") << sExpr.toString();
}

bool operator==(const SExpr &lhs, const SExpr &rhs) { return lhs.equals(rhs); }