#include "Num.hpp"
#include "cast.cpp"
#include <sstream>
#include <string>

using namespace sexpr;

Num::Num(const double val) : Atom(SExpr::Type::NUM), val(val) {}

std::string Num::toString() const {
  std::stringstream ss;
  ss << val;
  return ss.str();
}

bool Num::equals(const SExpr &other) const {
  if (isa<Num>(other)) {
    return val == cast<Num>(other).val;
  }
  return false;
}

bool Num::classOf(const SExpr &sExpr) { return sExpr.type == SExpr::Type::NUM; }

const std::string Num::typeName = "<Number>";