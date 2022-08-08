#include <memory>
#include <string>

#include "../../include/sexpr/SExprs.hpp"

using std::shared_ptr;
using std::string;

SExprs::SExprs(shared_ptr<SExpr> first, shared_ptr<SExpr> rest)
    : SExpr(SExpr::Type::SEXPRS), first(first), rest(rest) {}

string SExprs::toString() const {
  string str = "";
  str += first->type == SExpr::Type::SEXPRS ? "(" : "";
  str += first->toString();
  str += rest->type == SExpr::Type::NIL ? ")" : " " + rest->toString();
  return str;
}