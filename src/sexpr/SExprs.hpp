#ifndef LISP_SRC_SEXPR_SEXPRS_HPP_
#define LISP_SRC_SEXPR_SEXPRS_HPP_

#include "SExpr.hpp"
#include <memory>
#include <string>

namespace sexpr {

class SExprs final : public SExpr {
private:
  std::ostream &_serialize(std::ostream &o) const;

protected:
  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

public:
  SExpr *first;
  SExpr *rest;

  SExprs(SExpr *first, SExpr *rest);

  void fixupAddrs(const runtime::BreakTable &breakTable) override;

  static bool classOf(const SExpr *sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
