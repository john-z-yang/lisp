#include "ClosureAtom.hpp"
#include "../eval/EvalException.hpp"
#include "../eval/eval.hpp"
#include "NilAtom.hpp"
#include "cast.cpp"
#include <memory>
#include <sstream>
#include <string>

using std::make_shared;
using std::shared_ptr;
using std::string;
using std::stringstream;

ClosureAtom::ClosureAtom(Proc proc, const shared_ptr<Env> outerEnv,
                         const shared_ptr<SExpr> argNames)
    : Atom(SExpr::Type::CLOSURE), proc(proc), argNames(argNames),
      outerEnv(outerEnv) {}

shared_ptr<Env> ClosureAtom::bindArgs(shared_ptr<SExpr> args,
                                      shared_ptr<Env> curEnv) {
  shared_ptr<Env> env = std::make_shared<Env>(outerEnv);
  shared_ptr<SExpr> argVals = evalArgs(args, curEnv);
  if (isa<SExprs>(*argNames)) {
    shared_ptr<SExprs> argNamesIter = cast<SExprs>(argNames);
    shared_ptr<SExprs> argValsIter = cast<SExprs>(argVals);
    while (true) {
      string argName = cast<SymAtom>(argNamesIter->first)->val;
      env->def(argName, argValsIter->first);
      if (isa<NilAtom>(*argNamesIter->rest) &&
          isa<NilAtom>(*argValsIter->rest)) {
        break;
      } else if (isa<NilAtom>(*argNamesIter->rest) ||
                 isa<NilAtom>(*argValsIter->rest)) {
        handleArgMismatch(argNames, argVals);
      }
      argNamesIter = cast<SExprs>(argNamesIter->rest);
      argValsIter = cast<SExprs>(argValsIter->rest);
    }
  } else if (isa<SymAtom>(*argNames)) {
    string argName = cast<SymAtom>(argNames)->val;
    env->def(argName, evalArgs(args, curEnv));
  } else if (!isa<NilAtom>(*argVals)) {
    handleArgMismatch(argNames, argVals);
  }
  return env;
}

std::shared_ptr<SExpr> ClosureAtom::evalArgs(shared_ptr<SExpr> args,
                                             shared_ptr<Env> curEnv) {
  if (isa<NilAtom>(*args)) {
    return make_shared<NilAtom>();
  }
  shared_ptr<SExprs> sExprs = cast<SExprs>(args);
  return make_shared<SExprs>(eval(sExprs->first, curEnv),
                             evalArgs(sExprs->rest, curEnv));
}

void ClosureAtom::handleArgMismatch(shared_ptr<SExpr> argNames,
                                    shared_ptr<SExpr> argVals) {
  stringstream ss;
  ss << "Invalid number of arguments. Expected \"" << *argNames
     << "\", but got \"" << *argVals << "\".";
  throw EvalException(ss.str());
}

shared_ptr<SExpr> ClosureAtom::operator()(shared_ptr<SExpr> args,
                                          shared_ptr<Env> curEnv) {
  return proc(bindArgs(args, curEnv));
}

string ClosureAtom::toString() const { return "<closure>"; }

bool ClosureAtom::equals(const SExpr &other) const {
  if (isa<ClosureAtom>(other)) {
    return &proc == &dynamic_cast<const ClosureAtom &>(other).proc;
  }
  return false;
}

bool ClosureAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::CLOSURE;
}

const string ClosureAtom::typeName = "<closure>";