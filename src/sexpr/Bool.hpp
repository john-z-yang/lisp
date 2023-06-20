#ifndef LISP_SRC_SEXPR_BOOL_HPP_
#define LISP_SRC_SEXPR_BOOL_HPP_

#include "Atom.hpp"
#include "SExpr.hpp"
#include <memory>
#include <string>

namespace sexpr {

class Bool final : public Atom {
protected:
  Bool(const bool val);

  static Bool _true;
  static Bool _false;

  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  static Bool *getInstance(const bool val);

  const bool val;

  static bool toBool(const SExpr *const sExpr);
  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;

  Bool(Bool &other) = delete;
  void operator=(const Bool &) = delete;
};

} // namespace sexpr

#endif
