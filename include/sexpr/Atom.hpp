#ifndef LISP_INCLUDE_SEXPR_ATOM_H_
#define LISP_INCLUDE_SEXPR_ATOM_H_

#include "SExpr.hpp"

class Atom : public SExpr {
public:
  Atom(SExpr::Type type);
};

#endif