#ifndef LISP_SRC_SEXPR_STRING_HPP_
#define LISP_SRC_SEXPR_STRING_HPP_

#include "Atom.hpp"
#include <memory>
#include <string>

namespace sexpr {

class String final : public Atom {
public:
  using ValueType = std::string;

private:
  static ValueType escape(const ValueType literal);

protected:
  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

public:
  explicit String(const ValueType val);

  const ValueType val;
  const ValueType escaped;

  static bool classOf(const SExpr &sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
