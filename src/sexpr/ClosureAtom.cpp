#include "ClosureAtom.hpp"
#include "FnAtom.hpp"
#include "SExpr.hpp"
#include <sstream>

ClosureAtom::ClosureAtom(const std::shared_ptr<FnAtom> fnAtom)
    : Atom(SExpr::Type::CLOSURE), fnAtom(fnAtom) {}

std::string ClosureAtom::toString() const {
  std::stringstream ss;
  ss << "<Closure at " << this << ">";
  return ss.str();
}

bool ClosureAtom::equals(const SExpr &other) const { return this == &other; }

bool ClosureAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::CLOSURE;
}

const std::string ClosureAtom::typeName = "<closure>";
