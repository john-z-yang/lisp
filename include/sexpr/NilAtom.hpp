#ifndef LISP_INCLUDE_SEXPR_NILATOM_H_
#define LISP_INCLUDE_SEXPR_NILATOM_H_

#include "Atom.hpp"
#include <memory>

using std::shared_ptr;

class NilAtom : public Atom {
public:
  NilAtom();

  string toString() const;

  bool equals(const SExpr &other) const;

  static bool classOf(const SExpr &sExpr);
};

#endif