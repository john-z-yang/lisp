#ifndef LISP_INCLUDE_SEXPR_SEXPRS_H_
#define LISP_INCLUDE_SEXPR_SEXPRS_H_

#include "SExpr.hpp"
#include <memory>

using std::shared_ptr;

class SExprs : public SExpr {
public:
  shared_ptr<SExpr> first;
  shared_ptr<SExpr> rest;

  SExprs(shared_ptr<SExpr> first, shared_ptr<SExpr> rest);

  string toString() const;

  bool equals(const SExpr &other) const;

  static bool classOf(const SExpr &sExpr);
};

#endif