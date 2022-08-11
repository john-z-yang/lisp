#ifndef LISP_INCLUDE_SEXPR_INTATOM_H_
#define LISP_INCLUDE_SEXPR_INTATOM_H_

#include "Atom.hpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

class IntAtom : public Atom {
protected:
  string toString() const;
  bool equals(const SExpr &other) const;

public:
  const int val;

  IntAtom(const int val);

  static bool classOf(const SExpr &sExpr);
  static const string typeName;
};

#endif