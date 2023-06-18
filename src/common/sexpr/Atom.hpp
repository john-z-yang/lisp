#ifndef LISP_SRC_COMMON_SEXPR_ATOM_HPP_
#define LISP_SRC_COMMON_SEXPR_ATOM_HPP_

#include "SExpr.hpp"

class Atom : public SExpr {
public:
  Atom(SExpr::Type type);

  static bool classOf(const SExpr &sExpr);
};

#endif
