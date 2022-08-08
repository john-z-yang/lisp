#include <iostream>

#include "../../include/sexpr/SExpr.hpp"

using std::ostream;

SExpr::SExpr(SExpr::Type type) : type(type) {}

ostream &operator<<(ostream &o, const SExpr &sExpr) {
  return o << (sExpr.type == SExpr::Type::SEXPRS ? "(" : "")
           << sExpr.toString();
}