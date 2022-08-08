#ifndef LISP_INCLUDE_SEXPR_BOOLATOM_H_
#define LISP_INCLUDE_SEXPR_BOOLATOM_H_

#include <memory>
#include <string>

#include "Atom.hpp"
#include "SExpr.hpp"

using std::shared_ptr;
using std::string;

class BoolAtom : public Atom {
public:
  const bool val;

  BoolAtom(const bool val);

  BoolAtom(const shared_ptr<SExpr> sExpr);

  static bool cast(const shared_ptr<SExpr> sExpr);

  string toString() const;
};

#endif