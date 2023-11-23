#ifndef LISP_SRC_SEXPR_SEXPR_HPP_
#define LISP_SRC_SEXPR_SEXPR_HPP_

#include "../runtime/BreakTable.hpp"
#include <iostream>
#include <string>

namespace sexpr {

class SExpr {
  friend class SExprs;
  friend std::ostream &operator<<(std::ostream &o, const SExpr &sExpr);
  friend bool operator==(const SExpr &lhs, const SExpr &rhs);

public:
  using ID = uint64_t;

private:
  static ID curID;

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
    NATIVE_FN,
    UPVALUE
  };

  ID id;
  Type type;

  SExpr(SExpr::Type type);
  virtual ~SExpr();

  virtual void fixupAddrs(const runtime::BreakTable &breakTable) = 0;

  static bool classOf(const SExpr *sExpr);
  static std::string getTypeName();
};

std::ostream &operator<<(std::ostream &o, SExpr &sExpr);
bool operator==(SExpr &lhs, SExpr &rhs);

} // namespace sexpr

#endif
