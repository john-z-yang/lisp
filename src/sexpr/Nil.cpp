#include "Nil.hpp"
#include "Casting.hpp"
#include <cstddef>
#include <memory>
#include <string>

using namespace sexpr;

Nil::Nil() : Atom(SExpr::Type::NIL) {}

std::ostream &Nil::serialize(std::ostream &o) const { return o << "()"; }

bool Nil::equals(const SExpr &other) const { return isa<Nil>(other); }

Nil Nil::instance;

Nil *Nil::getInstance() { return &instance; }

bool Nil::classOf(const SExpr *sExpr) {
  return sExpr->type == SExpr::Type::NIL;
}

std::string Nil::getTypeName() { return "()"; }
