#ifndef LISP_SRC_SEXPR_FNATOM_HPP_
#define LISP_SRC_SEXPR_FNATOM_HPP_

#include "../code/Code.hpp"
#include "Atom.hpp"

class FnAtom final : public Atom {
public:
  FnAtom(int8_t arity);

  Code code;
  const int8_t arity;
  unsigned int numUpVals;

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;

  std::ostream &dissassemble(std::ostream &o);

protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;
};

#endif
