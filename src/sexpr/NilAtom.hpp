#ifndef LISP_SRC_SEXPR_NILATOM_HPP_
#define LISP_SRC_SEXPR_NILATOM_HPP_

#include "Atom.hpp"
#include <memory>

class NilAtom : public Atom {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  NilAtom();

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

#endif