#include "Fn.hpp"
#include "Cast.cpp"
#include <memory>
#include <sstream>

using namespace sexpr;

std::ostream &Fn::serialize(std::ostream &o) const {
  return o << "<Function at " << this << ">";
}

bool Fn::equals(const SExpr &other) const { return this == &other; }

Fn::Fn(const unsigned int numUpvals, const uint8_t arity, const bool variadic,
       const code::Code code)
    : Atom(SExpr::Type::FUNCTION), numUpvals(numUpvals), arity(arity),
      variadic(variadic), code(code) {}

std::ostream &Fn::dissassemble(std::ostream &o) const {
  o << "<Function at " << this << ", arity: " << unsigned(arity)
    << ", upvalues: " << numUpvals << ">" << std::endl
    << code << std::endl;
  for (auto i = code.consts.cbegin(); i != code.consts.end(); ++i) {
    if (isa<Fn>(i->get())) {
      const auto &fnAtom = cast<Fn>(i->get());
      fnAtom.dissassemble(o);
    }
  }
  return o;
}

bool Fn::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::FUNCTION;
}

std::string Fn::getTypeName() { return "<Function>"; }
