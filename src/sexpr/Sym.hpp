#ifndef LISP_SRC_SEXPR_SYM_HPP_
#define LISP_SRC_SEXPR_SYM_HPP_

#include "Atom.hpp"
#include <memory>
#include <string>

namespace sexpr {

class Sym final : public Atom {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  const std::string val;
  const size_t hash;

  explicit Sym(std::string val);

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;

  class HashFunction {
  public:
    size_t operator()(const Sym &sym) const;
  };

  class EqualFunction {
  public:
    bool operator()(const Sym &lhs, const Sym &rhs) const;
  };
};

} // namespace sexpr

#endif
