#ifndef LISP_INCLUDE_SEXPR_SEXPR_H_
#define LISP_INCLUDE_SEXPR_SEXPR_H_

#include <iostream>
#include <string>

using std::ostream;
using std::string;

class SExpr {
  friend class SExprs;
  friend ostream &operator<<(ostream &o, const SExpr &sExpr);
  friend bool operator==(SExpr &lhs, SExpr &rhs);

protected:
  virtual string toString() const = 0;
  virtual bool equals(const SExpr &other) const = 0;

public:
  enum Type { NIL, SYM, NUM, BOOL, SEXPRS, CLOSURE };

  const Type type;

  SExpr(SExpr::Type type);
};

ostream &operator<<(ostream &o, const SExpr &sExpr);
bool operator==(SExpr &lhs, SExpr &rhs);

#endif