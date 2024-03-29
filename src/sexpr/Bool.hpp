#ifndef LISP_SRC_SEXPR_BOOL_HPP_
#define LISP_SRC_SEXPR_BOOL_HPP_

#include "Atom.hpp"
#include "SExpr.hpp"
#include <memory>
#include <string>

namespace sexpr {

class Bool final : public Atom {
public:
  using ValueType = bool;

protected:
  explicit Bool(ValueType val);

  static Bool _true;
  static Bool _false;

  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

public:
  const ValueType val;

  static Bool *getInstance(const ValueType val);
  static bool toBool(const SExpr *sExpr);
  static bool classOf(const SExpr *sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
