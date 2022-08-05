#include "../../include/sexpr/SExprs.hpp"

SExprs::SExprs(std::shared_ptr<SExpr> first, std::shared_ptr<SExpr> rest)
    : SExpr(SExpr::Type::SEXPRS), first(first), rest(rest) {}

std::string SExprs::toString() const {
  return "(" + ((first) ? first->toString() : "nil") + ", " +
         ((rest) ? rest->toString() : "nil") + ")";
}