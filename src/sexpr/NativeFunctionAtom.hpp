#ifndef LISP_SRC_SEXPR_NATIVEFUNCTION_HPP_
#define LISP_SRC_SEXPR_NATIVEFUNCTION_HPP_

#include "Atom.hpp"
#include <functional>
#include <memory>
#include <vector>

class NativeFunctionAtom final : public Atom {
  typedef std::function<std::shared_ptr<SExpr>(
      std::vector<std::shared_ptr<SExpr>>::iterator, const uint8_t argc)>
      NativeFn;

protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  const NativeFn fn;
  const int argc;

  NativeFunctionAtom(NativeFn fn, const int argc);

  std::shared_ptr<SExpr> invoke(std::vector<std::shared_ptr<SExpr>>::iterator,
                                const unsigned int argc);
  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

#endif