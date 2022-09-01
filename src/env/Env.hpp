#ifndef LISP_SRC_ENV_ENV_HPP_
#define LISP_SRC_ENV_ENV_HPP_

#include "../eval/EvalException.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SymAtom.hpp"
#include <memory>
#include <unordered_map>

using std::shared_ptr;
using std::unordered_map;

class Env {
  typedef unordered_map<SymAtom, shared_ptr<SExpr>, SymAtom::HashFunction>
      SymTable;

private:
  SymTable symTable;
  SymTable &findSymTable(SymAtom &sym);

public:
  const shared_ptr<Env> outer;

  Env();
  Env(const shared_ptr<Env> outer);

  void def(SymAtom &sym, shared_ptr<SExpr> val);
  void def(SymAtom &&sym, shared_ptr<SExpr> val);

  void set(SymAtom &sym, shared_ptr<SExpr> val);

  shared_ptr<SExpr> find(SymAtom &sym);
  shared_ptr<SExpr> find(SymAtom &&sym);

  void clear();
};

void initEnv(shared_ptr<Env> env);

#endif