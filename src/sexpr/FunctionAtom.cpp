#include "FunctionAtom.hpp"
#include "../sexpr/cast.cpp"
#include <sstream>

FunctionAtom::FunctionAtom(int8_t arity)
    : Atom(SExpr::Type::FUNCTION), arity(arity) {}

std::string FunctionAtom::toString() const {
  std::stringstream ss;
  ss << "<Function at " << this << ">";
  return ss.str();
}

bool FunctionAtom::equals(const SExpr &other) const { return false; }

Code &FunctionAtom::getCode() { return code; }

std::ostream &FunctionAtom::dissassemble(std::ostream &o) {
  o << "<Function at " << this << "> with code:" << std::endl
    << code << std::endl;
  for (auto i = code.consts.begin(); i != code.consts.end(); ++i) {
    if (isa<FunctionAtom>(**i)) {
      cast<FunctionAtom>(*i)->dissassemble(o);
    }
  }
  return o;
}

bool FunctionAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::FUNCTION;
}

const std::string FunctionAtom::typeName = "<function>";