#include "FnAtom.hpp"
#include <memory>
#include <sstream>

FnAtom::FnAtom(const int8_t arity, const unsigned int numUpvals,
               const Code code)
    : Atom(SExpr::Type::FUNCTION), arity(arity), numUpvals(numUpvals),
      code(code) {}

std::string FnAtom::toString() const {
  std::stringstream ss;
  ss << "<Function at " << this << ">";
  return ss.str();
}

bool FnAtom::equals(const SExpr &other) const { return this == &other; }

std::ostream &FnAtom::dissassemble(std::ostream &o) const {
  o << "<Function at " << this << ", arity: " << unsigned(arity)
    << ", upvalues: " << numUpvals << ">" << std::endl
    << code << std::endl;
  for (auto i = code.consts.begin(); i != code.consts.end(); ++i) {
    if (const auto fnAtom = dynamic_cast<FnAtom *>(*i)) {
      fnAtom->dissassemble(o);
    }
  }
  return o;
}

bool FnAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::FUNCTION;
}

const std::string FnAtom::typeName = "<Function>";
