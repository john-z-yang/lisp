#ifndef LISP_SRC_COMMON_SEXPR_SYMATOM_HPP_
#define LISP_SRC_COMMON_SEXPR_SYMATOM_HPP_

#include "Atom.hpp"
#include <memory>
#include <string>

class SymAtom final : public Atom {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  const std::string val;

  SymAtom(std::string val);

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;

  class HashFunction {
  public:
    size_t operator()(const SymAtom *sym) const;
  };

  class EqualFunction {
  public:
    bool operator()(const SymAtom *lhs, const SymAtom *rhs) const;
  };
};

#endif
