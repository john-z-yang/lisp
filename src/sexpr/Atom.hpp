#ifndef LISP_SRC_SEXPR_ATOM_HPP_
#define LISP_SRC_SEXPR_ATOM_HPP_

#include "SExpr.hpp"

class Atom : public SExpr {
public:
  Atom(SExpr::Type type);
};

#endif