#include "ClosureAtom.hpp"
#include "../cast.cpp"
#include "FnAtom.hpp"
#include "SExpr.hpp"
#include <iomanip>
#include <sstream>

ClosureAtom::ClosureAtom(const std::shared_ptr<FnAtom> fnAtom)
    : Atom(SExpr::Type::CLOSURE), fnAtom(fnAtom) {}

void ClosureAtom::assertArity(const uint8_t argc) const {
  if (fnAtom->arity != -1 && argc != (uint8_t)fnAtom->arity) {
    std::stringstream ss;
    ss << "Invalid number of arguments. Expected " << unsigned(fnAtom->arity)
       << " arguments, but got " << unsigned(argc) << ".";
    throw std::invalid_argument(ss.str());
  }
}

std::ostream &ClosureAtom::dissassemble(std::ostream &o) {
  const unsigned int PADDING_WIDTH = 4;
  o << "<Closure at " << this << ">, instance of:" << std::endl
    << std::setw(PADDING_WIDTH) << "";
  fnAtom->dissassemble(o);
  return o << std::endl;
}

bool ClosureAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::CLOSURE;
}

const std::string ClosureAtom::typeName = "<Closure>";

std::string ClosureAtom::toString() const {
  std::stringstream ss;
  ss << "<Closure at " << this << ">";
  return ss.str();
}

bool ClosureAtom::equals(const SExpr &other) const {
  if (isa<ClosureAtom>(other)) {
    const auto closure = dynamic_cast<const ClosureAtom &>(other);
    if (fnAtom != closure.fnAtom) {
      return false;
    }
    return std::equal(
        upvalues.begin(), upvalues.end(), closure.upvalues.begin(),
        closure.upvalues.end(),
        [](std::shared_ptr<Upvalue> self, std::shared_ptr<Upvalue> other) {
          return **self->ptr == **other->ptr;
        });
  }
  return false;
}
