#ifndef LISP_SRC_SEXPR_NUM_HPP_
#define LISP_SRC_SEXPR_NUM_HPP_

#include "Atom.hpp"
#include <memory>
#include <string>

namespace sexpr {

class Num final : public Atom {
protected:
  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

public:
  using ValueType = double;

  explicit Num(const ValueType val);

  ValueType val;

  void fixupAddrs(const runtime::BreakTable &breakTable) override;

  static bool classOf(const SExpr *sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
