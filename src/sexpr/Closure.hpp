#ifndef LISP_SRC_SEXPR_CLOSURE_HPP_
#define LISP_SRC_SEXPR_CLOSURE_HPP_

#include "Atom.hpp"
#include "Prototype.hpp"
#include "SExpr.hpp"
#include "Upvalue.hpp"
#include <memory>
#include <vector>

namespace sexpr {

class Closure final : public Atom {
protected:
  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

public:
  explicit Closure(Prototype *proto);
  Closure(Prototype *proto, const std::vector<Upvalue *> upvalues);

  Prototype *proto;
  std::vector<Upvalue *> upvalues;

  void fixupAddrs(const runtime::BreakTable &breakTable) override;

  void assertArity(const uint8_t arity) const;
  std::ostream &dissassemble(std::ostream &o) const;

  static bool classOf(const SExpr *sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
