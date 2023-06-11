#ifndef LISP_SRC_VM_ENV_HPP_
#define LISP_SRC_VM_ENV_HPP_

#include "../common/sexpr/SExpr.hpp"
#include "../common/sexpr/SymAtom.hpp"
#include <memory>
#include <unordered_map>
#include <unordered_set>

class Env {
  typedef std::unordered_map<const SymAtom *, const SExpr *,
                             SymAtom::HashFunction, SymAtom::EqualFunction>
      SymVals;

  typedef std::unordered_set<const SymAtom *, SymAtom::HashFunction,
                             SymAtom::EqualFunction>
      Macros;

private:
  SymVals symTable;
  Macros macros;

public:
  void def(const SymAtom *sym, const SExpr *val);

  void set(const SymAtom *sym, const SExpr *val);

  const SymVals &getSymTable() const;

  const SExpr *find(const SymAtom *sym);

  void defMacro(const SymAtom *sym);

  bool isMacro(const SymAtom *sym);
};

#endif
