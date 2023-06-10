#ifndef LISP_SRC_COMMON_SEXPR_NATIVEFUNCTION_HPP_
#define LISP_SRC_COMMON_SEXPR_NATIVEFUNCTION_HPP_

#include "../../runtime/NatFnImpls.hpp"
#include "Atom.hpp"
#include <functional>
#include <memory>
#include <vector>

class NatFnAtom final : public Atom {
protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  NativeFn *fn;
  const int argc;

  NatFnAtom(NativeFn fn, const int argc);

  SExpr *invoke(std::vector<SExpr *>::iterator, const unsigned int argc,
                VM &vm);
  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

#endif
