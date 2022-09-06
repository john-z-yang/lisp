#include "ClosureAtom.hpp"
#include "../eval/EvalException.hpp"
#include "../eval/eval.hpp"
#include "NilAtom.hpp"
#include "cast.cpp"
#include <memory>
#include <sstream>
#include <string>

using std::make_shared;
using std::make_tuple;
using std::shared_ptr;
using std::string;
using std::stringstream;
using std::tuple;

ClosureAtom::ClosureAtom(Proc proc, const shared_ptr<Env> outerEnv,
                         const shared_ptr<SExpr> argNames)
    : ClosureAtom(proc, outerEnv, argNames, false) {}

ClosureAtom::ClosureAtom(Proc proc, const shared_ptr<Env> outerEnv,
                         const shared_ptr<SExpr> argNames, const bool isMacro)
    : Atom(SExpr::Type::CLOSURE), proc(proc), outerEnv(outerEnv),
      argNames(argNames), isMacro(isMacro) {}

shared_ptr<Env> ClosureAtom::bindArgs(shared_ptr<SExpr> args,
                                      shared_ptr<Env> curEnv) {
  shared_ptr<Env> env = std::make_shared<Env>(outerEnv);
  shared_ptr<SExpr> argVals = isMacro ? args : evalArgs(args, curEnv);
  if (isa<SExprs>(*argNames)) {
    shared_ptr<SExprs> argNamesIter = cast<SExprs>(argNames);
    shared_ptr<SExprs> argValsIter = cast<SExprs>(argVals);
    while (true) {
      env->def(*cast<SymAtom>(argNamesIter->first), argValsIter->first);
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
    env->def(cast<SymAtom>(argNames)->val, argVals);
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
  shared_ptr<SExpr> first = eval(sExprs->first, curEnv);
  shared_ptr<SExpr> rest = evalArgs(sExprs->rest, curEnv);
  return make_shared<SExprs>(first, rest);
}

void ClosureAtom::handleArgMismatch(shared_ptr<SExpr> argNames,
                                    shared_ptr<SExpr> argVals) {
  stringstream ss;
  ss << "Invalid number of arguments. Expected \"" << *argNames
     << "\", but got \"" << *argVals << "\".";
  throw EvalException(ss.str());
}

tuple<shared_ptr<SExpr>, shared_ptr<Env>>
ClosureAtom::expand(shared_ptr<SExpr> args, shared_ptr<Env> curEnv) {
  shared_ptr<Env> env = bindArgs(args, curEnv);
  if (isMacro) {
    return make_tuple(eval(proc(curEnv), env), curEnv);
  }
  return make_tuple(proc(env), env);
}

string ClosureAtom::toString() const {
  return isMacro ? "<macro>" : "<procedure>";
}

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