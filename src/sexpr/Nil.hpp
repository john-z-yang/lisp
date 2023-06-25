#ifndef LISP_SRC_SEXPR_NIL_HPP_
#define LISP_SRC_SEXPR_NIL_HPP_

#include "Atom.hpp"
#include <memory>

namespace sexpr {

class Nil final : public Atom {
protected:
  Nil();

  static Nil instance;

  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  static Nil &getInstance();

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;

  Nil(Nil &other) = delete;
  void operator=(const Nil &) = delete;
};

} // namespace sexpr

#endif
