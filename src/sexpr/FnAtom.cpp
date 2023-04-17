#include "FnAtom.hpp"
#include "../sexpr/cast.cpp"
#include <sstream>

FnAtom::FnAtom(int8_t arity)
    : Atom(SExpr::Type::FUNCTION), arity(arity), numUpVals(0) {}

std::string FnAtom::toString() const {
  std::stringstream ss;
  ss << "<Function at " << this << ">";
  return ss.str();
}

bool FnAtom::equals(const SExpr &other) const { return false; }

std::ostream &FnAtom::dissassemble(std::ostream &o) {
  o << "<Function at " << this << ", arity: " << unsigned(arity)
    << ", upvalues: " << numUpVals << ">" << std::endl
    << code << std::endl;
  for (auto i = code.consts.begin(); i != code.consts.end(); ++i) {
    if (isa<FnAtom>(**i)) {
      cast<FnAtom>(*i)->dissassemble(o);
    }
  }
  return o;
}

bool FnAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::FUNCTION;
}

const std::string FnAtom::typeName = "<function>";
