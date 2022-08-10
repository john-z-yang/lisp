#ifndef LISP_INCLUDE_SEXPR_SYMATOM_H_
#define LISP_INCLUDE_SEXPR_SYMATOM_H_

#include "Atom.hpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

class SymAtom : public Atom {
public:
  const string val;

  SymAtom(string val);

  string toString() const;

  bool equals(const SExpr &other) const;

  static bool classOf(SExpr &sExpr);
};

#endif