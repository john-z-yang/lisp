#ifndef LISP_SRC_SEXPR_SEXPRS_HPP_
#define LISP_SRC_SEXPR_SEXPRS_HPP_

#include "SExpr.hpp"
#include <memory>
#include <string>

namespace sexpr {

class SExprs final : public SExpr {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  const SExpr &first;
  const SExpr &rest;

  SExprs(const SExpr &first, const SExpr &rest);

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

} // namespace sexpr

#endif
