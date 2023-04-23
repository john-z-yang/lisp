#ifndef LISP_SRC_VM_ENV_HPP_
#define LISP_SRC_VM_ENV_HPP_

#include "../common/sexpr/SExpr.hpp"
#include "../common/sexpr/SymAtom.hpp"
#include <memory>
#include <unordered_map>

class Env {
  typedef std::unordered_map<SymAtom, std::shared_ptr<SExpr>,
                             SymAtom::HashFunction>
      SymVals;

private:
  SymVals symTable;

public:
  Env();

  void def(SymAtom &sym, std::shared_ptr<SExpr> val);

  void set(SymAtom &sym, std::shared_ptr<SExpr> val);

  const SymVals &getSymTable() const;

  std::shared_ptr<SExpr> find(SymAtom &sym);
};

#endif
