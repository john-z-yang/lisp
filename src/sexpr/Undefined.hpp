#ifndef LISP_SRC_SEXPR_UNDEFINED_HPP_
#define LISP_SRC_SEXPR_UNDEFINED_HPP_

#include "Atom.hpp"

namespace sexpr {

class Undefined final : public Atom {
protected:
  Undefined();

  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

  static Undefined instance;

public:
  void fixupAddrs(const runtime::BreakTable &breakTable) override;

  static Undefined *getInstance();
  static bool classOf(const SExpr *sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
