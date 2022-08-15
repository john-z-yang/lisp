#include "eval.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/ClosureAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/SymAtom.hpp"
#include "../sexpr/cast.cpp"
#include "grammar.hpp"
#include <memory>
#include <sstream>
#include <string>

using std::make_shared;
using std::string;
using std::stringstream;

shared_ptr<SExpr> get(const uint8_t n, shared_ptr<SExpr> sExpr) {
  shared_ptr<SExpr> it = cast<SExprs>(sExpr);
  for (uint8_t i = 0; i < n; ++i) {
    it = cast<SExprs>(it)->rest;
  }
  return it;
}

void handleSyntaxError(string expected, shared_ptr<SExpr> actual) {
  stringstream ss;
  ss << "Expected \"" << expected << ", but got \"" << *actual << "\".";
  throw EvalException(ss.str());
}

shared_ptr<SExpr> evalDef(shared_ptr<SExpr> sExpr, shared_ptr<Env> env) {
  string defSym;
  shared_ptr<SExpr> defSExpr;
  try {
    defSym = cast<SymAtom>(cast<SExprs>(get(defSymPos, sExpr))->first)->val;
    defSExpr = cast<SExprs>(get(defSExprPos, sExpr))->first;
    cast<NilAtom>(get(defNilPos, sExpr));
  } catch (EvalException ee) {
    handleSyntaxError(defGrammar, sExpr);
  }
  shared_ptr<SExpr> defVal = eval(defSExpr, env);
  env->def(defSym, defVal);
  return defVal;
}

shared_ptr<SExpr> evalSet(shared_ptr<SExpr> sExpr, shared_ptr<Env> env) {
  string setSym;
  shared_ptr<SExpr> setSExpr;
  try {
    setSym = cast<SymAtom>(cast<SExprs>(get(setSymPos, sExpr))->first)->val;
    setSExpr = cast<SExprs>(get(setSExprPos, sExpr))->first;
    cast<NilAtom>(get(setNilPos, sExpr));
  } catch (EvalException ee) {
    handleSyntaxError(setGrammar, sExpr);
  }
  shared_ptr<SExpr> setVal = eval(setSExpr, env);
  env->set(setSym, setVal);
  return setVal;
}

shared_ptr<SExpr> evalQuote(shared_ptr<SExpr> sExpr, shared_ptr<Env> env) {
  shared_ptr<SExpr> quoteVal;
  try {
    cast<NilAtom>(get(quoteNilPos, sExpr));
    quoteVal = cast<SExprs>(get(quoteArgPos, sExpr))->first;
  } catch (EvalException ee) {
    handleSyntaxError(quoteGrammar, sExpr);
  }
  return quoteVal;
}

shared_ptr<SExpr> evalIf(shared_ptr<SExpr> sExpr, shared_ptr<Env> env) {
  shared_ptr<BoolAtom> test;
  shared_ptr<SExpr> conseq;
  shared_ptr<SExpr> alt;
  try {
    test = make_shared<BoolAtom>(
        eval(cast<SExprs>(get(ifTestPos, sExpr))->first, env));
    conseq = cast<SExprs>(get(ifConseqPos, sExpr))->first;
    alt = cast<SExprs>(get(ifAltPos, sExpr))->first;
    cast<NilAtom>(get(ifNilPos, sExpr));
  } catch (EvalException ee) {
    handleSyntaxError(ifGrammar, sExpr);
  }
  return (test->val) ? eval(conseq, env) : eval(alt, env);
}

shared_ptr<SExpr> evalLambda(shared_ptr<SExpr> sExpr, shared_ptr<Env> env) {
  shared_ptr<SExpr> argNames;
  shared_ptr<SExpr> body;
  try {
    argNames = cast<SExprs>(get(lambdaArgPos, sExpr))->first;
    body = cast<SExprs>(get(lambdaBodyPos, sExpr))->first;
    cast<NilAtom>(get(lambdaNilPos, sExpr));
  } catch (EvalException ee) {
    handleSyntaxError(lambdaGrammar, sExpr);
  }
  return make_shared<ClosureAtom>(
      [body](shared_ptr<Env> env) { return eval(body, env); }, env, argNames);
}

shared_ptr<SExpr> eval(shared_ptr<SExpr> sExpr, shared_ptr<Env> env) {
  try {
    if (isa<NilAtom>(*sExpr) || isa<IntAtom>(*sExpr) || isa<BoolAtom>(*sExpr)) {
      return sExpr;
    } else if (isa<SymAtom>(*sExpr)) {
      return env->find(cast<SymAtom>(sExpr)->val);
    }
    shared_ptr<SExprs> sExprs = cast<SExprs>(sExpr);
    if (isa<SymAtom>(*sExprs->first)) {
      string sym = cast<SymAtom>(sExprs->first)->val;
      if (sym == "define") {
        return evalDef(sExpr, env);
      } else if (sym == "set!") {
        return evalSet(sExpr, env);
      } else if (sym == "quote") {
        return evalQuote(sExpr, env);
      } else if (sym == "if") {
        return evalIf(sExpr, env);
      } else if (sym == "lambda") {
        return evalLambda(sExpr, env);
      }
    }
    shared_ptr<ClosureAtom> closure =
        cast<ClosureAtom>(eval(sExprs->first, env));
    return (*closure)(sExprs->rest, env);
  } catch (EvalException ee) {
    ee.pushStackTrace(sExpr);
    throw ee;
  }
}
