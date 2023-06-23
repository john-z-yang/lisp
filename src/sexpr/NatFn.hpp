#ifndef LISP_SRC_SEXPR_NATFN_HPP_
#define LISP_SRC_SEXPR_NATFN_HPP_

#include "../runtime/CppFn.hpp"
#include "Atom.hpp"
#include <functional>
#include <memory>
#include <vector>

namespace sexpr {

class NatFn final : public Atom {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  runtime::CppFn *fn;
  const int argc;

  NatFn(runtime::CppFn fn, const int argc);

  const SExpr *invoke(std::vector<const SExpr *>::iterator,
                      const unsigned int argc, runtime::VM &vm) const;
  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

} // namespace sexpr

#endif
