#ifndef LISP_INCLUDE_SEXPR_SEXPRS_H_
#define LISP_INCLUDE_SEXPR_SEXPRS_H_

#include <memory>

#include "SExpr.hpp"

class SExprs : public SExpr {
public:
  std::shared_ptr<SExpr> first;
  std::shared_ptr<SExpr> rest;

  SExprs(std::shared_ptr<SExpr> first, std::shared_ptr<SExpr> rest);

  std::string toString() const;
};

#endif