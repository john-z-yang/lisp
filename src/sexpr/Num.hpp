#ifndef LISP_SRC_SEXPR_NUM_HPP_
#define LISP_SRC_SEXPR_NUM_HPP_

#include "Atom.hpp"
#include <memory>
#include <string>

namespace sexpr {

class Num final : public Atom {
protected:
  std::string toString() const override;
  bool equals(const SExpr &other) const override;

public:
  explicit Num(const double val);

  const double val;

  static bool classOf(const SExpr &sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
