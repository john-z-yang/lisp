#ifndef LISP_SRC_VM_ENV_HPP_
#define LISP_SRC_VM_ENV_HPP_

#include "../sexpr/SExpr.hpp"
#include "../sexpr/SymAtom.hpp"
#include <memory>
#include <unordered_map>

class Env {
  typedef std::unordered_map<SymAtom, std::shared_ptr<SExpr>,
                             SymAtom::HashFunction>
      SymVals;

public:
  Env();

  void def(SymAtom &sym, std::shared_ptr<SExpr> val);

  void set(SymAtom &sym, std::shared_ptr<SExpr> val);

  std::shared_ptr<SExpr> find(SymAtom &sym);

private:
  SymVals symTable;
};

#endif
