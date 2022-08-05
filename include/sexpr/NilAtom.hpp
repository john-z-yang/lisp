#ifndef LISP_INCLUDE_SEXPR_NILATOM_H_
#define LISP_INCLUDE_SEXPR_NILATOM_H_

#include "Atom.hpp"

class NilAtom : public Atom {
public:
  NilAtom();

  string toString() const;
};

#endif