#ifndef LISP_INCLUDE_SEXPR_SYMATOM_H_
#define LISP_INCLUDE_SEXPR_SYMATOM_H_

#include <string>

#include "Atom.hpp"

using std::string;

class SymAtom : public Atom {
public:
  const string val;

  SymAtom(string val);

  string toString() const;
};

#endif