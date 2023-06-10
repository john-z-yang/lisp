#ifndef LISP_SRC_COMMON_SEXPR_BOOLATOM_HPP_
#define LISP_SRC_COMMON_SEXPR_BOOLATOM_HPP_

#include "Atom.hpp"
#include "SExpr.hpp"
#include <memory>
#include <string>

class BoolAtom final : public Atom {
protected:
  BoolAtom(const bool val);

  static BoolAtom _true;
  static BoolAtom _false;

  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  static BoolAtom *getInstance(const bool val);

  const bool val;

  static bool toBool(SExpr *const sExpr);
  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;

  BoolAtom(BoolAtom &other) = delete;
  void operator=(const BoolAtom &) = delete;
};

#endif
