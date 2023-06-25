#ifndef LISP_SRC_SEXPR_NATFN_HPP_
#define LISP_SRC_SEXPR_NATFN_HPP_

#include "../runtime/CppFn.hpp"
#include "../runtime/StackIter.hpp"
#include "Atom.hpp"
#include <functional>
#include <memory>
#include <vector>

namespace sexpr {

class NatFn final : public Atom {
protected:
  std::string toString() const override;
  bool equals(const SExpr &other) const override;

public:
  runtime::CppFn *fn;
  const uint8_t argc;
  const bool isVariadic;

  NatFn(runtime::CppFn fn, const uint8_t argc, const bool isVariadic);

  const SExpr &invoke(runtime::StackIter params, const uint8_t incomingArgc,
                      runtime::VM &vm) const;
  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

} // namespace sexpr

#endif
