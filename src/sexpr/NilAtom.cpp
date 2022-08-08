#include "../../include/sexpr/NilAtom.hpp"
#include <string>

using std::string;

NilAtom::NilAtom() : Atom(SExpr::Type::NIL) {}

string NilAtom::toString() const { return "()"; }