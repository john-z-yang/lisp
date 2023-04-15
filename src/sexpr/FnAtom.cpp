#include "FnAtom.hpp"
#include "../sexpr/cast.cpp"
#include <sstream>

FnAtom::FnAtom(int8_t arity) : Atom(SExpr::Type::FUNCTION), arity(arity) {}

std::string FnAtom::toString() const {
  std::stringstream ss;
  ss << "<Function at " << this << ">";
  return ss.str();
}

bool FnAtom::equals(const SExpr &other) const { return false; }

Code &FnAtom::getCode() { return code; }

std::ostream &FnAtom::dissassemble(std::ostream &o) {
  o << "<Function at " << this << "> with code:" << std::endl
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
