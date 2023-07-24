#ifndef LISP_SRC_SEXPR_NATFN_HPP_
#define LISP_SRC_SEXPR_NATFN_HPP_

#include "../runtime/CPPFn.hpp"
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
  NatFn(runtime::CPPFn &fn, const uint8_t arity, const bool variadic);
  NatFn(runtime::CPPFn &fn, const uint8_t arity, const bool variadic,
        const bool abandonsCont);

  runtime::CPPFn &fn;
  const uint8_t arity;
  const bool variadic;
  const bool abandonsCont;

  const SExpr &invoke(runtime::StackIter params, const uint8_t argc,
                      runtime::VM &vm) const;

  static bool classOf(const SExpr &sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
