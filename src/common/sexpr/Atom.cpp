#include "Atom.hpp"

Atom::Atom(SExpr::Type type) : SExpr(type) {}

bool Atom::classOf(const SExpr &sExpr) {
  return sExpr.type != SExpr::Type::SEXPRS;
}
