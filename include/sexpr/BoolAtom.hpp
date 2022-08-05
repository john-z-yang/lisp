#ifndef LISP_INCLUDE_SEXPR_BOOLATOM_H_
#define LISP_INCLUDE_SEXPR_BOOLATOM_H_

#include "Atom.hpp"

class BoolAtom : public Atom {
public:
  const bool val;

  BoolAtom(const bool val);

  std::string toString() const;
};

#endif