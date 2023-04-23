#include "SExpr.hpp"
#include "../cast.cpp"
#include "SExprs.hpp"
#include <iostream>

SExpr::SExpr(SExpr::Type type) : type(type) {}

std::ostream &operator<<(std::ostream &o, const SExpr &sExpr) {
  return o << (isa<SExprs>(sExpr) ? "(" : "") << sExpr.toString();
}

bool operator==(const SExpr &lhs, const SExpr &rhs) { return lhs.equals(rhs); }
