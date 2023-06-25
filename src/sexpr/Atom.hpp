#ifndef LISP_SRC_SEXPR_ATOM_HPP_
#define LISP_SRC_SEXPR_ATOM_HPP_

#include "SExpr.hpp"

namespace sexpr {

class Atom : public SExpr {
public:
  explicit Atom(SExpr::Type type);

  static bool classOf(const SExpr &sExpr);
  static std::string getTypeName();
};

} // namespace sexpr

#endif
