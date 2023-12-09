#include "Atom.hpp"

using namespace sexpr;

Atom::Atom(SExpr::Type type) : SExpr(type) {}

bool Atom::classOf(const SExpr *sExpr) {
  return sExpr->type != SExpr::Type::SEXPRS;
}

std::string Atom::getTypeName() { return "<Atom>"; }
