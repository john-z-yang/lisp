#ifndef LISP_INCLUDE_SEXPR_INTATOM_H_
#define LISP_INCLUDE_SEXPR_INTATOM_H_

#include "Atom.hpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

class IntAtom : public Atom {
public:
  const int val;

  IntAtom(const int val);

  string toString() const;

  bool equals(const SExpr &other) const;

  static bool classOf(SExpr &sExpr);
};

#endif