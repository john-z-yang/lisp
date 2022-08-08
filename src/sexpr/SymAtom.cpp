#include "../../include/sexpr/SymAtom.hpp"
#include <string>

using std::string;

SymAtom::SymAtom(string val) : Atom(SExpr::Type::SYM), val(val) {}

string SymAtom::toString() const { return val; }