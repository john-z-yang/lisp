#ifndef LISP_INCLUDE_SEXPR_INTATOM_H_
#define LISP_INCLUDE_SEXPR_INTATOM_H_

#include <string>

#include "Atom.hpp"

using std::string;

class IntAtom : public Atom {
public:
  const int val;

  IntAtom(const int val);

  string toString() const;
};

#endif