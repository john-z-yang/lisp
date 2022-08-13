#ifndef LISP_SRC_SEXPR_SEXPRS_HPP_
#define LISP_SRC_SEXPR_SEXPRS_HPP_

#include "SExpr.hpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

class SExprs : public SExpr {
protected:
  string toString() const;
  bool equals(const SExpr &other) const;

public:
  shared_ptr<SExpr> first;
  shared_ptr<SExpr> rest;

  SExprs(shared_ptr<SExpr> first, shared_ptr<SExpr> rest);

  static bool classOf(const SExpr &sExpr);
  static const string typeName;
};

#endif