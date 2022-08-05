#include "../../include/sexpr/NilAtom.hpp"

using std::string;

NilAtom::NilAtom() : Atom(SExpr::Type::NIL) {}

string NilAtom::toString() const { return "()"; }