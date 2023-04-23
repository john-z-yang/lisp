#include "ClosureAtom.hpp"
#include "FnAtom.hpp"
#include "SExpr.hpp"
#include <sstream>

ClosureAtom::ClosureAtom(const std::shared_ptr<FnAtom> fnAtom)
    : Atom(SExpr::Type::CLOSURE), fnAtom(fnAtom) {}

std::ostream &ClosureAtom::dissassemble(std::ostream &o) {
  o << "<Closure at " << this << ">, instance of:" << std::endl;
  fnAtom->dissassemble(o);
  return o << std::endl;
}

bool ClosureAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::CLOSURE;
}

const std::string ClosureAtom::typeName = "<closure>";

std::string ClosureAtom::toString() const {
  std::stringstream ss;
  ss << "<Closure at " << this << ">";
  return ss.str();
}

bool ClosureAtom::equals(const SExpr &other) const { return this == &other; }
