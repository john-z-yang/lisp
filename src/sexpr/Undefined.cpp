#include "Undefined.hpp"
#include "Cast.cpp"
#include <string>

using namespace sexpr;

Undefined::Undefined() : Atom(SExpr::Type::UNDEFINED) {}

std::ostream &Undefined::serialize(std::ostream &o) const {
  return o << "#<undefined>";
}

bool Undefined::equals(const SExpr &other) const {
  return isa<Undefined>(other);
}

Undefined Undefined::instance;

Undefined &Undefined::getInstance() { return instance; }

bool Undefined::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::UNDEFINED;
}

std::string Undefined::getTypeName() { return "#<undefined>"; }
