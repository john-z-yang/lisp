#include "Atom.hpp"

using namespace sexpr;

Atom::Atom(SExpr::Type type) : SExpr(type) {}

bool Atom::classOf(const SExpr &sExpr) {
  return sExpr.type != SExpr::Type::SEXPRS;
}

const std::string Atom::typeName = "<Atom>";
