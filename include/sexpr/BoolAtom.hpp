#ifndef LISP_INCLUDE_SEXPR_BOOLATOM_H_
#define LISP_INCLUDE_SEXPR_BOOLATOM_H_

#include "Atom.hpp"
#include "SExpr.hpp"

using std::shared_ptr;
using std::string;

class BoolAtom : public Atom {
private:
  bool cast(const shared_ptr<SExpr> sExpr);

public:
  const bool val;

  BoolAtom(const bool val);

  BoolAtom(const shared_ptr<SExpr> sExpr);

  string toString() const;
};

#endif