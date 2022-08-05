#include "../../include/sexpr/ClosureAtom.hpp"
#include "../../include/repl/repl.hpp"

ClosureAtom::ClosureAtom(Proc proc, std::shared_ptr<Env> outerEnv,
                         const std::shared_ptr<SExprs> argNames,
                         const bool capture)
    : Atom(SExpr::Type::CLOSURE), proc(proc), argNames(argNames),
      outerEnv(outerEnv), capture(capture) {}

std::shared_ptr<Env> ClosureAtom::bindArgs(std::shared_ptr<SExprs> argVals,
                                           std::shared_ptr<Env> curEnv) {
  std::shared_ptr<Env> env = std::make_shared<Env>(capture ? curEnv : outerEnv);
  std::shared_ptr<SExprs> argNamesIter = argNames;
  while (argNamesIter) {
    std::string argName =
        std::dynamic_pointer_cast<SymAtom>(argNamesIter->first)->val;
    std::shared_ptr<SExpr> argVal = eval(argVals->first, curEnv);
    argNamesIter = std::dynamic_pointer_cast<SExprs>(argNamesIter->rest);
    argVals = std::dynamic_pointer_cast<SExprs>(argVals->rest);
    env->symTable[argName] = argVal;
  }
  return env;
}

std::shared_ptr<SExpr> ClosureAtom::operator()(std::shared_ptr<SExprs> argVals,
                                               std::shared_ptr<Env> curEnv) {
  return proc(bindArgs(argVals, curEnv));
}

std::string ClosureAtom::toString() const { return "<closure>"; }