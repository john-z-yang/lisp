#ifndef LISP_SRC_SEXPR_CLOSURE_HPP_
#define LISP_SRC_SEXPR_CLOSURE_HPP_

#include "../runtime/Upvalue.hpp"
#include "Atom.hpp"
#include "Prototype.hpp"
#include "SExpr.hpp"
#include <memory>
#include <vector>

namespace sexpr {

class Closure final : public Atom {
protected:
  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

public:
  explicit Closure(const Prototype &fnAtom);
  Closure(const Prototype &fnAtom,
          const std::vector<std::shared_ptr<runtime::Upvalue>> upvalues);

  const Prototype &fn;
  const std::vector<std::shared_ptr<runtime::Upvalue>> upvalues;

  void assertArity(const uint8_t arity) const;
  std::ostream &dissassemble(std::ostream &o) const;

  static bool classOf(const SExpr &sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
