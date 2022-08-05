#include "../../include/sexpr/SExprs.hpp"

using std::shared_ptr;
using std::string;

SExprs::SExprs(shared_ptr<SExpr> first, shared_ptr<SExpr> rest)
    : SExpr(SExpr::Type::SEXPRS), first(first), rest(rest) {}

string SExprs::toString() const {
  return "(" + ((first) ? first->toString() : "nil") + ", " +
         ((rest) ? rest->toString() : "nil") + ")";
}