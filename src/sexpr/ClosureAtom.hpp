#ifndef LISP_SRC_SEXPR_CLOSUREATOM_HPP_
#define LISP_SRC_SEXPR_CLOSUREATOM_HPP_

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
  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;

  const std::shared_ptr<FnAtom> fnAtom;
  std::vector<std::shared_ptr<SExpr>> upValues;
};

#endif
