#include "Closure.hpp"
#include "Fn.hpp"
#include "SExpr.hpp"
#include "cast.cpp"
#include <iomanip>
#include <sstream>

using namespace sexpr;
using namespace runtime;

Closure::Closure(const Fn *fnAtom)
    : Atom(SExpr::Type::CLOSURE), fnAtom(fnAtom) {}

Closure::Closure(const Fn *fnAtom,
                 const std::vector<std::shared_ptr<Upvalue>> upvalues)
    : Atom(SExpr::Type::CLOSURE), fnAtom(fnAtom), upvalues(upvalues) {}

void Closure::assertArity(const uint8_t argc) const {
  if (fnAtom->arity != -1 && argc != (uint8_t)fnAtom->arity) {
    std::stringstream ss;
    ss << "Invalid number of arguments. Expected " << unsigned(fnAtom->arity)
       << " arguments, but got " << unsigned(argc) << ".";
    throw std::invalid_argument(ss.str());
  }
}

std::ostream &Closure::dissassemble(std::ostream &o) const {
  const unsigned int PADDING_WIDTH = 4;
  o << "<Closure at " << this << ">, instance of:" << std::endl
    << std::setw(PADDING_WIDTH) << "";
  fnAtom->dissassemble(o);
  return o << std::endl;
}

bool Closure::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::CLOSURE;
}

const std::string Closure::typeName = "<Closure>";

std::string Closure::toString() const {
  std::stringstream ss;
  ss << "<Closure at " << this << ">";
  return ss.str();
}

bool Closure::equals(const SExpr &other) const {
  if (isa<Closure>(other)) {
    const auto closure = cast<Closure>(other);
    if (fnAtom != closure.fnAtom) {
      return false;
    }
    return std::equal(
        upvalues.begin(), upvalues.end(), closure.upvalues.begin(),
        closure.upvalues.end(),
        [](const auto &self, const auto &other) { return *self == *other; });
  }
  return false;
}
