#ifndef LISP_SRC_SEXPR_NIL_HPP_
#define LISP_SRC_SEXPR_NIL_HPP_

#include "Atom.hpp"
#include <memory>

namespace sexpr {

class Nil final : public Atom {
protected:
  Nil();

  static Nil instance;

  std::string toString() const override;
  bool equals(const SExpr &other) const override;

public:
  static Nil &getInstance();

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

} // namespace sexpr

#endif
