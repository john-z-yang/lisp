#ifndef LISP_INCLUDE_SEXPR_CLOSUREATOM_H_
#define LISP_INCLUDE_SEXPR_CLOSUREATOM_H_

#include "../../include/env/Env.hpp"
#include "../../include/sexpr/Atom.hpp"
#include "../../include/sexpr/SExprs.hpp"
#include "../../include/sexpr/SymAtom.hpp"
#include <functional>
#include <memory>

using std::function;
using std::shared_ptr;

class ClosureAtom : public Atom {
  typedef function<shared_ptr<SExpr>(shared_ptr<Env>)> Proc;

public:
  Proc proc;
  const shared_ptr<Env> outerEnv;
  const shared_ptr<SExpr> argNames;

  ClosureAtom(Proc proc, const shared_ptr<Env> outerEnv,
              const shared_ptr<SExpr> argNames);

  std::shared_ptr<SExpr> evalArgs(shared_ptr<SExpr> args,
                                  shared_ptr<Env> curEnv);

  shared_ptr<Env> bindArgs(shared_ptr<SExpr> args, shared_ptr<Env> curEnv);

  shared_ptr<SExpr> operator()(shared_ptr<SExpr> args, shared_ptr<Env> curEnv);

  string toString() const;
};

#endif