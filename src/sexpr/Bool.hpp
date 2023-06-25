#ifndef LISP_SRC_SEXPR_BOOL_HPP_
#define LISP_SRC_SEXPR_BOOL_HPP_

#include "Atom.hpp"
#include "SExpr.hpp"
#include <memory>
#include <string>

namespace sexpr {

class Bool final : public Atom {
protected:
  explicit Bool(const bool val);

  static Bool _true;
  static Bool _false;

  std::string toString() const override;
  bool equals(const SExpr &other) const override;

public:
  const bool val;

  static Bool &getInstance(const bool val);
  static bool toBool(const SExpr &sExpr);
  static bool classOf(const SExpr &sExpr);
  static constexpr std::string getTypeName() { return "<Boolean>"; }
};

} // namespace sexpr

#endif
