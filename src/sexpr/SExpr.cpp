#include "SExpr.hpp"
#include <iostream>

using namespace sexpr;

sexpr::SExpr::ID sexpr::SExpr::curID = 0;

SExpr::SExpr(SExpr::Type type) : id(curID++), type(type) {}

SExpr::~SExpr() {}

bool SExpr::classOf(const SExpr *) { return true; }

std::string SExpr::getTypeName() { return "<S-expression>"; }

std::ostream &sexpr::operator<<(std::ostream &o, const SExpr &sExpr) {
  return sExpr.serialize(o);
}

bool sexpr::operator==(const SExpr &lhs, const SExpr &rhs) {
  return lhs.equals(rhs);
}
