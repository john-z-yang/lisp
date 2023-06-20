#include "Fn.hpp"
#include "cast.cpp"
#include <memory>
#include <sstream>

using namespace sexpr;

Fn::Fn(const int8_t arity, const unsigned int numUpvals, const Code code)
    : Atom(SExpr::Type::FUNCTION), arity(arity), numUpvals(numUpvals),
      code(code) {}

std::string Fn::toString() const {
  std::stringstream ss;
  ss << "<Function at " << this << ">";
  return ss.str();
}

bool Fn::equals(const SExpr &other) const { return this == &other; }

std::ostream &Fn::dissassemble(std::ostream &o) const {
  o << "<Function at " << this << ", arity: " << unsigned(arity)
    << ", upvalues: " << numUpvals << ">" << std::endl
    << code << std::endl;
  for (auto i = code.consts.begin(); i != code.consts.end(); ++i) {
    if (const auto fnAtom = dynCast<Fn>(*i)) {
      fnAtom->dissassemble(o);
    }
  }
  return o;
}

bool Fn::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::FUNCTION;
}

const std::string Fn::typeName = "<Function>";
