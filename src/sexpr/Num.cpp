#include "Num.hpp"
#include "Cast.cpp"
#include <iomanip>
#include <limits>
#include <sstream>
#include <string>

using namespace sexpr;

std::ostream &Num::serialize(std::ostream &o) const {
  return o << std::setprecision(
                  std::numeric_limits<Num::ValueType>::max_digits10
              )
           << val;
}

bool Num::equals(const SExpr &other) const {
  if (isa<Num>(other)) {
    return val == cast<Num>(other).val;
  }
  return false;
}

Num::Num(const Num::ValueType val) : Atom(SExpr::Type::NUM), val(val) {}

void Num::fixupAddrs(const runtime::BreakTable &) {}

bool Num::classOf(const SExpr *sExpr) {
  return sExpr->type == SExpr::Type::NUM;
}

std::string Num::getTypeName() { return "<Number>"; }
