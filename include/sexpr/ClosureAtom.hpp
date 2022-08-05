#ifndef LISP_INCLUDE_SEXPR_CLOSUREATOM_H_
#define LISP_INCLUDE_SEXPR_CLOSUREATOM_H_

#include "../../include/env/Env.hpp"
#include "../../include/sexpr/Atom.hpp"
#include "../../include/sexpr/SExprs.hpp"
#include "../../include/sexpr/SymAtom.hpp"

class ClosureAtom : public Atom {
  typedef std::function<std::shared_ptr<SExpr>(std::shared_ptr<Env>)> Proc;

public:
  Proc proc;
  const std::shared_ptr<SExprs> argNames;
  std::shared_ptr<Env> outerEnv;
  const bool capture;

  ClosureAtom(Proc proc, std::shared_ptr<Env> outerEnv,
              const std::shared_ptr<SExprs> argNames, const bool capture);

  std::shared_ptr<Env> bindArgs(std::shared_ptr<SExprs> argVals,
                                std::shared_ptr<Env> curEnv);

  std::shared_ptr<SExpr> operator()(std::shared_ptr<SExprs> argVals,
                                    std::shared_ptr<Env> curEnv);

  std::string toString() const;
};

#endif