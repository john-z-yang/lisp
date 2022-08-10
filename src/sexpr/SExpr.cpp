#include "../../include/sexpr/SExpr.hpp"
#include "../../include/sexpr/SExprs.hpp"
#include "cast.cpp"
#include <iostream>

using std::ostream;

SExpr::SExpr(SExpr::Type type) : type(type) {}

ostream &operator<<(ostream &o, const SExpr &sExpr) {
  return o << (sExpr.type == SExpr::SEXPRS ? "(" : "") << sExpr.toString();
}

bool operator==(SExpr &lhs, SExpr &rhs) { return lhs.equals(rhs); }