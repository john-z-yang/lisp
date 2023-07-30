#ifndef LISP_SRC_SEXPR_SEXPR_HPP_
#define LISP_SRC_SEXPR_SEXPR_HPP_

#include <iostream>
#include <string>

namespace sexpr {

class SExpr {
  friend class SExprs;
  friend std::ostream &operator<<(std::ostream &o, const SExpr &sExpr);
  friend bool operator==(const SExpr &lhs, const SExpr &rhs);

protected:
  virtual std::ostream &serialize(std::ostream &o) const = 0;
  virtual bool equals(const SExpr &other) const = 0;

public:
  enum Type {
    UNDEFINED,
    NIL,
    SYM,
    NUM,
    BOOL,
    STR,
    SEXPRS,
    PROTO,
    CLOSURE,
    NATIVE_FN
  };

  const Type type;

  SExpr(SExpr::Type type);
  virtual ~SExpr();

  SExpr(const SExpr &) = delete;
  SExpr(const SExpr &&) = delete;
  SExpr &operator=(const SExpr &) = delete;
};

std::ostream &operator<<(std::ostream &o, const SExpr &sExpr);
bool operator==(const SExpr &lhs, const SExpr &rhs);

} // namespace sexpr

#endif
