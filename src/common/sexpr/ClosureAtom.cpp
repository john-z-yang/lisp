#include "ClosureAtom.hpp"
#include "FnAtom.hpp"
#include "SExpr.hpp"
#include <iomanip>
#include <sstream>

ClosureAtom::ClosureAtom(const std::shared_ptr<FnAtom> fnAtom)
    : Atom(SExpr::Type::CLOSURE), fnAtom(fnAtom) {}

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

bool ClosureAtom::equals(const SExpr &other) const { return this == &other; }
