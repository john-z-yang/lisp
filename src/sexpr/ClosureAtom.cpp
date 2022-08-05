#include "../../include/sexpr/ClosureAtom.hpp"
#include "../../include/repl/repl.hpp"

using std::dynamic_pointer_cast;
using std::shared_ptr;
using std::string;

ClosureAtom::ClosureAtom(Proc proc, shared_ptr<Env> outerEnv,
                         const shared_ptr<SExprs> argNames, const bool capture)
    : Atom(SExpr::Type::CLOSURE), proc(proc), argNames(argNames),
      outerEnv(outerEnv), capture(capture) {}

shared_ptr<Env> ClosureAtom::bindArgs(shared_ptr<SExprs> argVals,
                                      shared_ptr<Env> curEnv) {
  shared_ptr<Env> env = std::make_shared<Env>(capture ? curEnv : outerEnv);
  shared_ptr<SExprs> argNamesIter = argNames;
  while (argNamesIter) {
    string argName = dynamic_pointer_cast<SymAtom>(argNamesIter->first)->val;
    shared_ptr<SExpr> argVal = eval(argVals->first, curEnv);
    argNamesIter = dynamic_pointer_cast<SExprs>(argNamesIter->rest);
    argVals = dynamic_pointer_cast<SExprs>(argVals->rest);
    env->symTable[argName] = argVal;
  }
  return env;
}

shared_ptr<SExpr> ClosureAtom::operator()(shared_ptr<SExprs> argVals,
                                          shared_ptr<Env> curEnv) {
  return proc(bindArgs(argVals, curEnv));
}

string ClosureAtom::toString() const { return "<closure>"; }