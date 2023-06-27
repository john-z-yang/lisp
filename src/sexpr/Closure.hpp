#ifndef LISP_SRC_SEXPR_CLOSURE_HPP_
#define LISP_SRC_SEXPR_CLOSURE_HPP_

#include "../runtime/Upvalue.hpp"
#include "Atom.hpp"
#include "Fn.hpp"
#include "SExpr.hpp"
#include <memory>
#include <vector>

namespace sexpr {

class Closure final : public Atom {
protected:
  std::string toString() const override;
  bool equals(const SExpr &other) const override;

public:
  explicit Closure(const Fn &fnAtom);
  Closure(const Fn &fnAtom,
          const std::vector<std::shared_ptr<runtime::Upvalue>> upvalues);

  const Fn &fnAtom;
  const std::vector<std::shared_ptr<runtime::Upvalue>> upvalues;

  void assertArity(const uint8_t arity) const;
  std::ostream &dissassemble(std::ostream &o) const;

  static bool classOf(const SExpr &sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
