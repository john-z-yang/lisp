#include "Closure.hpp"
#include "Cast.cpp"
#include "Prototype.hpp"
#include "SExpr.hpp"
#include "Upvalue.hpp"
#include <iomanip>
#include <sstream>

using namespace sexpr;
using namespace runtime;

std::ostream &Closure::serialize(std::ostream &o) const {
  return o << "<Closure at " << this << ">";
}

bool Closure::equals(const SExpr &other) const {
  if (isa<Closure>(other)) {
    const auto &closure = cast<Closure>(other);
    if (proto != closure.proto) {
      return false;
    }
    return std::equal(
        upvalues.cbegin(),
        upvalues.cend(),
        closure.upvalues.cbegin(),
        closure.upvalues.cend(),
        [](const auto &self, const auto &other) {
          return std::as_const(*self) == std::as_const(*other);
        }
    );
  }
  return false;
}

Closure::Closure(Prototype *proto) : Atom(SExpr::Type::CLOSURE), proto(proto) {}

Closure::Closure(Prototype *proto, const std::vector<Upvalue *> upvalues)
    : Atom(SExpr::Type::CLOSURE), proto(proto), upvalues(upvalues) {}

void Closure::fixupAddrs(const runtime::BreakTable &breakTable) {
  proto = cast<Prototype>(breakTable.get(proto));
  for (auto &upvalue : upvalues) {
    upvalue = cast<Upvalue>(breakTable.get(upvalue));
  }
}

void Closure::assertArity(const uint8_t argc) const {
  if ((!proto->variadic && proto->arity != argc) ||
      (proto->variadic && argc < proto->arity)) {
    std::stringstream ss;
    ss << "Invalid number of arguments. Expected " << unsigned(proto->arity);
    if (proto->variadic) {
      ss << " or more";
    }
    ss << " arguments, but got " << unsigned(argc) << ".";
    throw std::invalid_argument(ss.str());
  }
}

std::ostream &Closure::dissassemble(std::ostream &o) const {
  const unsigned int PADDING_WIDTH = 4;
  o << "<Closure (ID: " << id << ")>, instance of:" << std::endl
    << std::setw(PADDING_WIDTH) << "";
  proto->dissassemble(o);
  return o;
}

bool Closure::classOf(const SExpr *sExpr) {
  return sExpr->type == SExpr::Type::CLOSURE;
}

std::string Closure::getTypeName() { return "<Closure>"; }
