#include "Closure.hpp"
#include "Cast.cpp"
#include "Fn.hpp"
#include "SExpr.hpp"
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
    if (fn != closure.fn) {
      return false;
    }
    return std::equal(
        upvalues.cbegin(), upvalues.cend(), closure.upvalues.cbegin(),
        closure.upvalues.cend(),
        [](const auto &self, const auto &other) { return *self == *other; });
  }
  return false;
}

Closure::Closure(const Fn &fnAtom) : Atom(SExpr::Type::CLOSURE), fn(fnAtom) {}

Closure::Closure(const Fn &fnAtom,
                 const std::vector<std::shared_ptr<Upvalue>> upvalues)
    : Atom(SExpr::Type::CLOSURE), fn(fnAtom), upvalues(upvalues) {}

void Closure::assertArity(const uint8_t argc) const {
  if ((!fn.variadic && fn.arity != argc) || (fn.variadic && argc < fn.arity)) {
    std::stringstream ss;
    ss << "Invalid number of arguments. Expected " << unsigned(fn.arity);
    if (fn.variadic) {
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
  fn.dissassemble(o);
  return o;
}

bool Closure::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::CLOSURE;
}

std::string Closure::getTypeName() { return "<Closure>"; }
