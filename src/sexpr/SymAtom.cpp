#include "../../include/sexpr/SymAtom.hpp"

SymAtom::SymAtom(std::string val) : Atom(SExpr::Type::SYM), val(val) {}

std::string SymAtom::toString() const { return val; }