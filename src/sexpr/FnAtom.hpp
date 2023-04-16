#ifndef LISP_SRC_SEXPR_FNATOM_HPP_
#define LISP_SRC_SEXPR_FNATOM_HPP_

#include "../code/Code.hpp"
#include "Atom.hpp"

class FnAtom final : public Atom {
  Code code;

protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  FnAtom(int8_t arity);
  const int8_t arity;
  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
  Code &getCode();
  std::ostream &dissassemble(std::ostream &o);
};

#endif
