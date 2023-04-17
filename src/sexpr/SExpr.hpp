#ifndef LISP_SRC_SEXPR_SEXPR_HPP_
#define LISP_SRC_SEXPR_SEXPR_HPP_

#include <iostream>
#include <string>

class SExpr {
  friend class SExprs;
  friend std::ostream &operator<<(std::ostream &o, const SExpr &sExpr);
  friend bool operator==(const SExpr &lhs, const SExpr &rhs);

public:
  enum Type { NIL, SYM, NUM, BOOL, STR, SEXPRS, FUNCTION, CLOSURE, NATIVE_FN };

  const Type type;

  SExpr(SExpr::Type type);

protected:
  virtual std::string toString() const = 0;
  virtual bool equals(const SExpr &other) const = 0;
};

std::ostream &operator<<(std::ostream &o, const SExpr &sExpr);
bool operator==(const SExpr &lhs, const SExpr &rhs);

#endif
