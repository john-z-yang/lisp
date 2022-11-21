#ifndef LISP_SRC_SEXPR_CLOSUREATOM_HPP_
#define LISP_SRC_SEXPR_CLOSUREATOM_HPP_

#include "../env/Env.hpp"
#include "Atom.hpp"
#include "SExprs.hpp"
#include "SymAtom.hpp"
#include <functional>
#include <memory>
#include <tuple>

class ClosureAtom : public Atom {
  typedef std::function<std::shared_ptr<SExpr>(std::shared_ptr<Env>)> Proc;

  // private:
  //   void handleArgMismatch(std::shared_ptr<SExpr> argNames,
  //                          std::shared_ptr<SExpr> argVals);
  //   std::shared_ptr<SExpr> evalArgs(std::shared_ptr<SExpr> args,
  //                                   std::shared_ptr<Env> curEnv);
  //   std::shared_ptr<Env> bindArgs(std::shared_ptr<SExpr> args,
  //                                 std::shared_ptr<Env> curEnv);

protected:
  std::string toString() const;
  bool equals(const SExpr &other) const;

public:
  Proc proc;
  const std::shared_ptr<Env> outerEnv;
  const std::shared_ptr<SExpr> argNames;
  const bool isMacro;

  ClosureAtom(Proc proc, const std::shared_ptr<Env> outerEnv,
              const std::shared_ptr<SExpr> argNames);
  ClosureAtom(Proc proc, const std::shared_ptr<Env> outerEnv,
              const std::shared_ptr<SExpr> argNames, const bool isMacro);

  std::tuple<std::shared_ptr<SExpr>, std::shared_ptr<Env>>
  expand(std::shared_ptr<SExpr> args, std::shared_ptr<Env> curEnv);

  static bool classOf(const SExpr &sExpr);
  static const std::string typeName;
};

#endif