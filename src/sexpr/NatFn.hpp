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
  NatFn(
      fn::CPPFn *fn,
      std::string name,
      const uint8_t arity,
      const bool variadic,
      const bool abandonsCont = false
  );

  fn::CPPFn *fn;
  std::string name;
  uint8_t arity;
  bool variadic;
  bool abandonsCont;

  void fixupAddrs(const runtime::BreakTable &breakTable) override;

  SExpr *
  invoke(runtime::StackIter params, const uint8_t argc, runtime::VM &vm) const;

  static bool classOf(const SExpr *sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
