#ifndef LISP_SRC_SEXPR_BOOLATOM_HPP_
#define LISP_SRC_SEXPR_BOOLATOM_HPP_

#include "Atom.hpp"
#include "SExpr.hpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

class BoolAtom : public Atom {
protected:
  string toString() const;
  bool equals(const SExpr &other) const;

public:
  const bool val;

  BoolAtom(const bool val);
  BoolAtom(const shared_ptr<SExpr> sExpr);

  static bool toBool(const shared_ptr<SExpr> sExpr);
  static bool classOf(const SExpr &sExpr);
  static const string typeName;
};

#endif