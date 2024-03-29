#include "Closure.hpp"
#include "Casting.hpp"
#include "Prototype.hpp"
#include "SExpr.hpp"
#include <iomanip>
#include <sstream>

using namespace sexpr;
using namespace runtime;

std::ostream &Closure::serialize(std::ostream &o) const {
  return o << "<Closure at " << this << ">";
}

bool Closure::equals(const SExpr &other) const {
  if (const auto closure = dynCast<Closure>(other)) {
    if (proto != closure->get().proto) {
      return false;
    }
    return std::equal(
        upvalues.cbegin(),
        upvalues.cend(),
        closure->get().upvalues.cbegin(),
        closure->get().upvalues.cend(),
        [](const auto &self, const auto &other) { return *self == *other; }
    );
  }
  return false;
}

Closure::Closure(const Prototype *proto)
    : Atom(SExpr::Type::CLOSURE), proto(proto) {}

Closure::Closure(
    const Prototype *proto, const std::vector<std::shared_ptr<Upvalue>> upvalues
)
    : Atom(SExpr::Type::CLOSURE), proto(proto), upvalues(upvalues) {}

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
  o << "<Closure at " << this << ">, instance of:" << std::endl
    << std::setw(PADDING_WIDTH) << "";
  proto->dissassemble(o);
  return o;
}

bool Closure::classOf(const SExpr *sExpr) {
  return sExpr->type == SExpr::Type::CLOSURE;
}

std::string Closure::getTypeName() { return "<Closure>"; }
