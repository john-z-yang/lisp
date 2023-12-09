#ifndef LISP_SRC_SEXPR_NATFN_HPP_
#define LISP_SRC_SEXPR_NATFN_HPP_

#include "../fn/CPPFn.hpp"
#include "../runtime/StackIter.hpp"
#include "Atom.hpp"
#include <functional>
#include <memory>
#include <vector>

namespace sexpr {

class NatFn final : public Atom {
protected:
  std::ostream &serialize(std::ostream &o) const override;
  bool equals(const SExpr &other) const override;

public:
  NatFn(fn::CPPFn &fn, const uint8_t arity, const bool variadic);
  NatFn(
      fn::CPPFn &fn,
      const uint8_t arity,
      const bool variadic,
      const bool abandonsCont
  );

  fn::CPPFn &fn;
  const uint8_t arity;
  const bool variadic;
  const bool abandonsCont;

  const SExpr *
  invoke(runtime::StackIter params, const uint8_t argc, runtime::VM &vm) const;

  static bool classOf(const SExpr *sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
