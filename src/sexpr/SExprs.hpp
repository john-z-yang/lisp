#ifndef LISP_SRC_SEXPR_SEXPRS_HPP_
#define LISP_SRC_SEXPR_SEXPRS_HPP_

#include "SExpr.hpp"
#include <memory>
#include <string>

class SExprs final : public SExpr {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  std::shared_ptr<SExpr> first;
  std::shared_ptr<SExpr> rest;

  SExprs(std::shared_ptr<SExpr> first, std::shared_ptr<SExpr> rest);

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

#endif
