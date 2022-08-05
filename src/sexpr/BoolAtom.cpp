#include "../../include/sexpr/BoolAtom.hpp"

using std::string;

BoolAtom::BoolAtom(const bool val) : Atom(SExpr::Type::BOOL), val(val) {}

string BoolAtom::toString() const { return (val) ? "#t" : "#f"; }