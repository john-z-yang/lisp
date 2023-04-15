#ifndef LISP_SRC_SEXPR_STRINGATOM_HPP_
#define LISP_SRC_SEXPR_STRINGATOM_HPP_

#include "Atom.hpp"
#include <memory>
#include <string>

class StringAtom final : public Atom {
private:
  static std::string unescape(const std::string literal);

protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  const std::string literal;
  const std::string unescaped;

  StringAtom(const std::string literal);

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

#endif
