#ifndef LISP_SRC_SEXPR_SEXPR_HPP_
#define LISP_SRC_SEXPR_SEXPR_HPP_

#include <iostream>
#include <string>

using std::ostream;
using std::string;

class SExpr {
  friend class SExprs;
  friend ostream &operator<<(ostream &o, const SExpr &sExpr);
  friend bool operator==(const SExpr &lhs, const SExpr &rhs);

protected:
  virtual string toString() const = 0;
  virtual bool equals(const SExpr &other) const = 0;

public:
  enum Type { NIL, SYM, NUM, BOOL, STR, SEXPRS, CLOSURE };

  const Type type;

  SExpr(SExpr::Type type);
};

ostream &operator<<(ostream &o, const SExpr &sExpr);
bool operator==(const SExpr &lhs, const SExpr &rhs);

#endif