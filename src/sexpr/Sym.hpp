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
  class HashFunction {
  public:
    size_t operator()(const Sym &sym) const;
  };

  class EqualFunction {
  public:
    bool operator()(const Sym &lhs, const Sym &rhs) const;
  };

  using ValueType = std::string;

  const ValueType val;
  const size_t hash;

  explicit Sym(const ValueType val);

  static bool classOf(const SExpr &sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
