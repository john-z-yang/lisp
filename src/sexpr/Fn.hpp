#ifndef LISP_SRC_SEXPR_FN_HPP_
#define LISP_SRC_SEXPR_FN_HPP_

#include "../common/Code.hpp"
#include "Atom.hpp"

namespace sexpr {

class Fn final : public Atom {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  Fn(const int8_t arity, const unsigned int numUpvals, const Code code);

  const int8_t arity;
  const unsigned int numUpvals;
  const Code code;

  std::ostream &dissassemble(std::ostream &o) const;

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

} // namespace sexpr

#endif
