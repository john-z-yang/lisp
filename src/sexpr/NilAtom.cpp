#include "../../include/sexpr/NilAtom.hpp"
#include <string>

using std::string;

NilAtom::NilAtom() : Atom(SExpr::Type::NIL) {}

string NilAtom::toString() const { return "()"; }

bool NilAtom::equals(const SExpr &other) const {
  return other.type == SExpr::Type::NIL;
}