#include "../../include/sexpr/IntAtom.hpp"

using std::string;
using std::to_string;

IntAtom::IntAtom(const int val) : Atom(SExpr::Type::NUM), val(val) {}

string IntAtom::toString() const { return to_string(val); }