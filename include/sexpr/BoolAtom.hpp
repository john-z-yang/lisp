#ifndef LISP_INCLUDE_SEXPR_BOOLATOM_H_
#define LISP_INCLUDE_SEXPR_BOOLATOM_H_

#include "Atom.hpp"
#include "SExpr.hpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

class BoolAtom : public Atom {
public:
  const bool val;

  BoolAtom(const bool val);

  BoolAtom(const shared_ptr<SExpr> sExpr);

  string toString() const;

  bool equals(const SExpr &other) const;

  static bool toBool(const shared_ptr<SExpr> sExpr);

  static bool classOf(SExpr &sExpr);
};

#endif