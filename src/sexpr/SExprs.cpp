#include "SExprs.hpp"
#include "Cast.cpp"
#include "Nil.hpp"
#include <cstddef>
#include <memory>
#include <sstream>
#include <string>

using namespace sexpr;

std::string SExprs::toString() const {
  std::string str = "";
  str += isa<SExprs>(first) ? "(" : "";
  str += first.toString();
  if (isa<Nil>(rest)) {
    str += ")";
  } else if (!isa<SExprs>(rest)) {
    str += " . " + rest.toString() + ")";
  } else {
    str += isa<Nil>(rest) ? ")" : " " + rest.toString();
  }
  return str;
}

bool SExprs::equals(const SExpr &other) const {
  if (isa<SExprs>(other)) {
    const auto &sExprs = cast<SExprs>(other);
    return first.equals(sExprs.first) && rest.equals(sExprs.rest);
  }
  return false;
}

SExprs::SExprs(const SExpr &first, const SExpr &rest)
    : SExpr(SExpr::Type::SEXPRS), first(first), rest(rest) {}

bool SExprs::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::SEXPRS;
}

std::string SExprs::getTypeName() { return "<S-expressions>"; }
