#ifndef LISP_SRC_SEXPR_BOOLATOM_HPP_
#define LISP_SRC_SEXPR_BOOLATOM_HPP_

#include "Atom.hpp"
#include "SExpr.hpp"
#include <memory>
#include <string>

class BoolAtom final : public Atom {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  const bool val;

  BoolAtom(const bool val);
  BoolAtom(const std::shared_ptr<SExpr> sExpr);

  static bool toBool(const std::shared_ptr<SExpr> sExpr);
  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

#endif