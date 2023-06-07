#ifndef LISP_SRC_COMMON_SEXPR_CLOSUREATOM_HPP_
#define LISP_SRC_COMMON_SEXPR_CLOSUREATOM_HPP_

#include "../../runtime/Upvalue.hpp"
#include "Atom.hpp"
#include "FnAtom.hpp"
#include "SExpr.hpp"
#include <memory>
#include <vector>

class ClosureAtom final : public Atom {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  ClosureAtom(const std::shared_ptr<FnAtom> fnAtom);

  void assertArity(const uint8_t arity) const;

  const std::shared_ptr<FnAtom> fnAtom;
  std::vector<std::shared_ptr<Upvalue>> upvalues;

  std::ostream &dissassemble(std::ostream &o);

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

#endif
