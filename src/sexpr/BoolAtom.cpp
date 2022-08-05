#include "../../include/sexpr/BoolAtom.hpp"

BoolAtom::BoolAtom(const bool val) : Atom(SExpr::Type::BOOL), val(val) {}

std::string BoolAtom::toString() const { return (val) ? "#t" : "#f"; }