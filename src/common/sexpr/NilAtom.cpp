#include "NilAtom.hpp"
#include "../cast.cpp"
#include <cstddef>
#include <memory>
#include <string>

NilAtom::NilAtom() : Atom(SExpr::Type::NIL) {}

std::string NilAtom::toString() const { return "()"; }

bool NilAtom::equals(const SExpr &other) const { return isa<NilAtom>(other); }

NilAtom *NilAtom::getInstance() {
  static NilAtom instance;
  return &instance;
}

bool NilAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::NIL;
}

const std::string NilAtom::typeName = "()";
