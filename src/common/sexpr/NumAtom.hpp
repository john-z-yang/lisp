#ifndef LISP_SRC_COMMON_SEXPR_INTATOM_HPP_
#define LISP_SRC_COMMON_SEXPR_INTATOM_HPP_

#include "Atom.hpp"
#include <memory>
#include <string>

class NumAtom final : public Atom {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  const double val;

  NumAtom(const double val);

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

#endif
