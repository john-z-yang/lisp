#ifndef LISP_INCLUDE_SEXPR_BOOLATOM_H_
#define LISP_INCLUDE_SEXPR_BOOLATOM_H_

#include "Atom.hpp"

using std::string;

class BoolAtom : public Atom {
public:
  const bool val;

  BoolAtom(const bool val);

  string toString() const;
};

#endif