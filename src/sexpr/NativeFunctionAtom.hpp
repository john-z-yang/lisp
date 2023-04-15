#ifndef LISP_SRC_SEXPR_NATIVEFUNCTION_HPP_
#define LISP_SRC_SEXPR_NATIVEFUNCTION_HPP_

#include "../vm/NativeFns.hpp"
#include "Atom.hpp"
#include <functional>
#include <memory>
#include <vector>

class NativeFunctionAtom final : public Atom {

protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  NativeFn *fn;
  const int argc;

  NativeFunctionAtom(NativeFn fn, const int argc);

  std::shared_ptr<SExpr> invoke(std::vector<std::shared_ptr<SExpr>>::iterator,
                                const unsigned int argc);
  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

#endif
