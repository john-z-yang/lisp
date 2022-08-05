#ifndef LISP_INCLUDE_SEXPR_SEXPRS_H_
#define LISP_INCLUDE_SEXPR_SEXPRS_H_

#include <memory>

#include "SExpr.hpp"

using std::shared_ptr;

class SExprs : public SExpr {
public:
  shared_ptr<SExpr> first;
  shared_ptr<SExpr> rest;

  SExprs(shared_ptr<SExpr> first, shared_ptr<SExpr> rest);

  string toString() const;
};

#endif