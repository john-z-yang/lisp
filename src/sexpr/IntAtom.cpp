#include "../../include/sexpr/IntAtom.hpp"

IntAtom::IntAtom(const int val) : Atom(SExpr::Type::NUM), val(val) {}

std::string IntAtom::toString() const { return std::to_string(val); }