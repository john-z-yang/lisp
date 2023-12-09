#ifndef LISP_SRC_SEXPR_NIL_HPP_
#define LISP_SRC_SEXPR_NIL_HPP_

#include "Atom.hpp"
#include <memory>

namespace sexpr {

class Nil final : public Atom {
protected:
  Nil();

  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

  static Nil instance;

public:
  static Nil *getInstance();
  static bool classOf(const SExpr *sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
