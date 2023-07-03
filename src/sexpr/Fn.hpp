#ifndef LISP_SRC_SEXPR_FN_HPP_
#define LISP_SRC_SEXPR_FN_HPP_

#include "../code/Code.hpp"
#include "Atom.hpp"

namespace sexpr {

class Fn final : public Atom {
protected:
  std::string toString() const override;
  bool equals(const SExpr &other) const override;

public:
  Fn(const unsigned int numUpvals, const uint8_t arity, const bool variadic,
     const code::Code code);

  const unsigned int numUpvals;
  const uint8_t arity;
  const bool variadic;
  const code::Code code;

  std::ostream &dissassemble(std::ostream &o) const;

  static bool classOf(const SExpr &sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
