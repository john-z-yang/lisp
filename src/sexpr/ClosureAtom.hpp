#ifndef LISP_SRC_SEXPR_CLOSUREATOM_HPP_
#define LISP_SRC_SEXPR_CLOSUREATOM_HPP_

#include "../env/Env.hpp"
#include "Atom.hpp"
#include "SExprs.hpp"
#include "SymAtom.hpp"
#include <functional>
#include <memory>

using std::function;
using std::shared_ptr;

class ClosureAtom : public Atom {
  typedef function<shared_ptr<SExpr>(shared_ptr<Env>)> Proc;

private:
  void handleArgMismatch(shared_ptr<SExpr> argNames, shared_ptr<SExpr> argVals);
  std::shared_ptr<SExpr> evalArgs(shared_ptr<SExpr> args,
                                  shared_ptr<Env> curEnv);
  shared_ptr<Env> bindArgs(shared_ptr<SExpr> args, shared_ptr<Env> curEnv);

protected:
  string toString() const;
  bool equals(const SExpr &other) const;

public:
  Proc proc;
  const shared_ptr<Env> outerEnv;
  const shared_ptr<SExpr> argNames;

  ClosureAtom(Proc proc, const shared_ptr<Env> outerEnv,
              const shared_ptr<SExpr> argNames);

  shared_ptr<SExpr> operator()(shared_ptr<SExpr> args, shared_ptr<Env> curEnv);

  static bool classOf(const SExpr &sExpr);
  static const string typeName;
};

#endif