#include "Prototype.hpp"
#include "Cast.cpp"
#include <memory>
#include <sstream>

using namespace sexpr;

std::ostream &Prototype::serialize(std::ostream &o) const {
  return o << "<Function (ID: " << id << ")>";
}

bool Prototype::equals(const SExpr &other) const { return this == &other; }

Prototype::Prototype(
    const unsigned int numUpvals,
    const uint8_t arity,
    const bool variadic,
    const code::Code code
)
    : Atom(SExpr::Type::PROTO),
      numUpvals(numUpvals),
      arity(arity),
      variadic(variadic),
      code(code) {}

void Prototype::fixupAddrs(const runtime::BreakTable &breakTable) {
  code.fixupAddrs(breakTable);
}

std::ostream &Prototype::dissassemble(std::ostream &o) const {
  o << "<Function (ID: " << id << "), arity: " << unsigned(arity)
    << ", upvalues: " << numUpvals << ">" << std::endl
    << code << std::endl;
  for (auto i = code.consts.cbegin(); i != code.consts.end(); ++i) {
    if (isa<Prototype>(*i)) {
      const auto proto = cast<Prototype>(*i);
      proto->dissassemble(o);
    }
  }
  return o;
}

bool Prototype::classOf(const SExpr *sExpr) {
  return sExpr->type == SExpr::Type::PROTO;
}

std::string Prototype::getTypeName() { return "<Prototype>"; }
