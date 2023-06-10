#ifndef LISP_SRC_COMMON_SEXPR_NILATOM_HPP_
#define LISP_SRC_COMMON_SEXPR_NILATOM_HPP_

#include "Atom.hpp"
#include <memory>

class NilAtom final : public Atom {
protected:
  NilAtom();

  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  static NilAtom *getInstance();

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;

  NilAtom(NilAtom &other) = delete;
  void operator=(const NilAtom &) = delete;
};

#endif
