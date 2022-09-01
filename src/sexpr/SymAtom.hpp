#ifndef LISP_SRC_SEXPR_SYMATOM_HPP_
#define LISP_SRC_SEXPR_SYMATOM_HPP_

#include "Atom.hpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

class SymAtom : public Atom {
protected:
  string toString() const;
  bool equals(const SExpr &other) const;

public:
  const string val;

  SymAtom(string val);

  static bool classOf(const SExpr &sExpr);
  static const string typeName;

  class HashFunction {
  public:
    size_t operator()(const SymAtom &sym) const;
  };
};

#endif