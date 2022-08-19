#include "SExprs.hpp"
#include "NilAtom.hpp"
#include "cast.cpp"
#include <memory>
#include <sstream>
#include <string>

using std::shared_ptr;
using std::string;
using std::stringstream;

SExprs::SExprs(shared_ptr<SExpr> first, shared_ptr<SExpr> rest)
    : SExpr(SExpr::Type::SEXPRS), first(first), rest(rest) {}

string SExprs::toString() const {
  string str = "";
  str += isa<SExprs>(*first) ? "(" : "";
  str += first->toString();
  if (isa<NilAtom>(*rest)) {
    str += ")";
  } else if (!isa<SExprs>(*rest)) {
    str += " . " + rest->toString() + ")";
  } else {
    str += isa<NilAtom>(*rest) ? ")" : " " + rest->toString();
  }
  return str;
}

bool SExprs::equals(const SExpr &other) const {
  if (isa<SExprs>(other)) {
    const SExprs &sExprs = dynamic_cast<const SExprs &>(other);
    return first->equals(*sExprs.first) && rest->equals(*sExprs.rest);
  }
  return false;
}

bool SExprs::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::SEXPRS;
}

const string SExprs::typeName = "One or more symbolic expressions";