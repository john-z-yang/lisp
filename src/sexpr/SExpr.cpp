#include "../../include/sexpr/SExpr.hpp"

SExpr::SExpr(SExpr::Type type) : type(type) {}

std::ostream &operator<<(std::ostream &o, const SExpr &sExpr) {
  return o << sExpr.toString();
}