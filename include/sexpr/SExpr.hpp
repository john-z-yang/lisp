#ifndef LISP_INCLUDE_SEXPR_SEXPR_H_
#define LISP_INCLUDE_SEXPR_SEXPR_H_

#include <iostream>
#include <string>

class SExpr {
public:
  enum Type { NIL, SYM, NUM, BOOL, SEXPRS, CLOSURE };

  const Type type;

  SExpr(SExpr::Type type);

  virtual std::string toString() const = 0;
  friend std::ostream &operator<<(std::ostream &o, const SExpr &sExpr);
};

std::ostream &operator<<(std::ostream &o, const SExpr &sExpr);
#endif