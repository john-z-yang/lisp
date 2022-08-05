#ifndef LISP_INCLUDE_SEXPR_SEXPR_H_
#define LISP_INCLUDE_SEXPR_SEXPR_H_

#include <iostream>
#include <string>

using std::ostream;
using std::string;

class SExpr {
public:
  enum Type { NIL, SYM, NUM, BOOL, SEXPRS, CLOSURE };

  const Type type;

  SExpr(SExpr::Type type);

  virtual string toString() const = 0;
  friend ostream &operator<<(ostream &o, const SExpr &sExpr);
};

ostream &operator<<(ostream &o, const SExpr &sExpr);
#endif