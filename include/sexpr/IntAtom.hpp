#ifndef LISP_INCLUDE_SEXPR_INTATOM_H_
#define LISP_INCLUDE_SEXPR_INTATOM_H_

#include "Atom.hpp"

class IntAtom : public Atom {
public:
  const int val;

  IntAtom(const int val);

  std::string toString() const;
};

#endif