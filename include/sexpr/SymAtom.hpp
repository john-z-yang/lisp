#ifndef LISP_INCLUDE_SEXPR_SYMATOM_H_
#define LISP_INCLUDE_SEXPR_SYMATOM_H_

#include "Atom.hpp"

class SymAtom : public Atom {
public:
  const std::string val;

  SymAtom(std::string val);

  std::string toString() const;
};

#endif