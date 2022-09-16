#ifndef LISP_SRC_SEXPR_STRINGATOM_HPP_
#define LISP_SRC_SEXPR_STRINGATOM_HPP_

#include "Atom.hpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

class StringAtom : public Atom {
private:
  static string unescape(const string literal);

protected:
  string toString() const;
  bool equals(const SExpr &other) const;

public:
  const string literal;
  const string unescaped;

  StringAtom(const string literal);

  static bool classOf(const SExpr &sExpr);
  static const string typeName;
};

#endif