#ifndef LISP_SRC_SEXPR_STRING_HPP_
#define LISP_SRC_SEXPR_STRING_HPP_

#include "Atom.hpp"
#include <memory>
#include <string>

namespace sexpr {

class String final : public Atom {
private:
  static std::string unescape(const std::string literal);

protected:
  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

public:
  explicit String(const std::string literal);

  const std::string literal;
  const std::string unescaped;

  static bool classOf(const SExpr &sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
