#ifndef LISP_SRC_SEXPR_SYM_HPP_
#define LISP_SRC_SEXPR_SYM_HPP_

#include "Atom.hpp"
#include <memory>
#include <string>

namespace sexpr {

class Sym final : public Atom {
protected:
  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

public:
  using ValueType = std::string;

  const ValueType val;

  explicit Sym(const ValueType val);

  static bool classOf(const SExpr *sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
