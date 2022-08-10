#include "../../include/sexpr/ClosureAtom.hpp"
#include "../../include/repl/repl.hpp"
#include "../../include/sexpr/NilAtom.hpp"
#include "cast.cpp"
#include <memory>
#include <string>

using std::dynamic_pointer_cast;
using std::make_shared;
using std::shared_ptr;
using std::string;

ClosureAtom::ClosureAtom(Proc proc, const shared_ptr<Env> outerEnv,
                         const shared_ptr<SExpr> argNames)
    : Atom(SExpr::Type::CLOSURE), proc(proc), argNames(argNames),
      outerEnv(outerEnv) {}

shared_ptr<Env> ClosureAtom::bindArgs(shared_ptr<SExpr> args,
                                      shared_ptr<Env> curEnv) {
  shared_ptr<Env> env = std::make_shared<Env>(outerEnv);
  if (isa<NilAtom>(*argNames)) {
    return env;
  }
  shared_ptr<SExpr> argVals = evalArgs(args, curEnv);
  if (isa<SExprs>(*argNames)) {
    shared_ptr<SExprs> argNamesIter = dynamic_pointer_cast<SExprs>(argNames);
    shared_ptr<SExprs> argValsIter = dynamic_pointer_cast<SExprs>(argVals);
    while (argNamesIter) {
      string argName = dynamic_pointer_cast<SymAtom>(argNamesIter->first)->val;
      env->symTable[argName] = argValsIter->first;
      argNamesIter = dynamic_pointer_cast<SExprs>(argNamesIter->rest);
      argValsIter = dynamic_pointer_cast<SExprs>(argValsIter->rest);
    }
  } else {
    string argName = dynamic_pointer_cast<SymAtom>(argNames)->val;
    env->symTable[argName] = evalArgs(args, curEnv);
  }
  return env;
}

std::shared_ptr<SExpr> ClosureAtom::evalArgs(shared_ptr<SExpr> args,
                                             shared_ptr<Env> curEnv) {
  if (isa<NilAtom>(*args)) {
    return make_shared<NilAtom>();
  }
  shared_ptr<SExprs> sExprs = dynamic_pointer_cast<SExprs>(args);
  return make_shared<SExprs>(eval(sExprs->first, curEnv),
                             evalArgs(sExprs->rest, curEnv));
}

shared_ptr<SExpr> ClosureAtom::operator()(shared_ptr<SExpr> args,
                                          shared_ptr<Env> curEnv) {
  return proc(bindArgs(args, curEnv));
}

string ClosureAtom::toString() const { return "<closure>"; }

bool ClosureAtom::equals(const SExpr &other) const {
  if (other.type != SExpr::Type::CLOSURE) {
    return false;
  }
  return &proc == &dynamic_cast<const ClosureAtom &>(other).proc;
}

bool ClosureAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::CLOSURE;
}