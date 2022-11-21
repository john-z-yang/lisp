#ifndef LISP_SRC_ENV_ENV_HPP_
#define LISP_SRC_ENV_ENV_HPP_

#include "../eval/EvalException.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SymAtom.hpp"
#include <memory>
#include <unordered_map>

class Env {
  typedef std::unordered_map<SymAtom, std::shared_ptr<SExpr>,
                             SymAtom::HashFunction>
      SymTable;

private:
  SymTable symTable;
  SymTable &findSymTable(SymAtom &sym);

public:
  const std::shared_ptr<Env> outer;

  Env();
  Env(const std::shared_ptr<Env> outer);

  void def(SymAtom &sym, std::shared_ptr<SExpr> val);
  void def(SymAtom &&sym, std::shared_ptr<SExpr> val);

  void set(SymAtom &sym, std::shared_ptr<SExpr> val);

  std::shared_ptr<SExpr> find(SymAtom &sym);
  std::shared_ptr<SExpr> find(SymAtom &&sym);

  void clear();
};

void initEnv(std::shared_ptr<Env> env);

#endif