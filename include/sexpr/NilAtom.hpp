#ifndef LISP_INCLUDE_SEXPR_NILATOM_H_
#define LISP_INCLUDE_SEXPR_NILATOM_H_

#include "Atom.hpp"
#include <memory>

using std::shared_ptr;

class NilAtom : public Atom {
protected:
  string toString() const;
  bool equals(const SExpr &other) const;

public:
  NilAtom();

  static bool classOf(const SExpr &sExpr);
  static const string typeName;
};

#endif