#include "SExpr.hpp"
#include "SExprs.hpp"
#include "cast.cpp"
#include <iostream>

using std::ostream;

SExpr::SExpr(SExpr::Type type) : type(type) {}

ostream &operator<<(ostream &o, const SExpr &sExpr) {
  return o << (isa<SExprs>(sExpr) ? "(" : "") << sExpr.toString();
}

bool operator==(SExpr &lhs, SExpr &rhs) { return lhs.equals(rhs); }