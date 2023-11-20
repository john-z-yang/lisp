#include "SExprs.hpp"
#include "Atom.hpp"
#include "Cast.cpp"
#include "Nil.hpp"
#include <cstddef>
#include <memory>
#include <sstream>
#include <string>

using namespace sexpr;

std::ostream &SExprs::serialize(std::ostream &o) const {
  return _serialize(o << "(");
}

std::ostream &SExprs::_serialize(std::ostream &o) const {
  first->serialize(o);
  if (isa<Nil>(rest)) {
    return o << ")";
  } else if (isa<Atom>(rest)) {
    return rest->serialize(o << " . ") << ")";
  }
  const auto sexprs = cast<SExprs>(rest);
  return sexprs->_serialize(o << " ");
}

bool SExprs::equals(const SExpr &other) const {
  if (isa<SExprs>(other)) {
    const auto &sExprs = cast<SExprs>(other);
    return first->equals(*sExprs.first) && rest->equals(*sExprs.rest);
  }
  return false;
}

SExprs::SExprs(const SExpr *first, const SExpr *rest)
    : SExpr(SExpr::Type::SEXPRS), first(first), rest(rest) {}

bool SExprs::classOf(const SExpr *sExpr) {
  return sExpr->type == SExpr::Type::SEXPRS;
}

std::string SExprs::getTypeName() { return "<S-expressions>"; }
