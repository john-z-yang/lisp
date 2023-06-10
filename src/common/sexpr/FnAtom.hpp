#ifndef LISP_SRC_COMMON_SEXPR_FNATOM_HPP_
#define LISP_SRC_COMMON_SEXPR_FNATOM_HPP_

#include "../Code.hpp"
#include "Atom.hpp"

class FnAtom final : public Atom {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  FnAtom(int8_t arity);

  Code code;
  const int8_t arity;
  unsigned int numUpVals;

  std::ostream &dissassemble(std::ostream &o) const;

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

#endif
