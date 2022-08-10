#include "../../include/sexpr/NilAtom.hpp"
#include "cast.cpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

NilAtom::NilAtom() : Atom(SExpr::Type::NIL) {}

string NilAtom::toString() const { return "()"; }

bool NilAtom::equals(const SExpr &other) const {
  return other.type == SExpr::Type::NIL;
}

bool NilAtom::classOf(SExpr &sExpr) { return sExpr.type == SExpr::Type::NIL; }