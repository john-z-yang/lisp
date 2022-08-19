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

shared_ptr<SExpr> evalQuote(shared_ptr<SExpr> sExpr, shared_ptr<Env> env) {
  shared_ptr<SExpr> quoteArg;
  try {
    cast<NilAtom>(get(quoteNilPos, sExpr));
    quoteArg = cast<SExprs>(get(quoteArgPos, sExpr))->first;
  } catch (EvalException ee) {
    handleSyntaxError(quoteGrammar, sExpr);
  }
  return quoteArg;
}

shared_ptr<SExpr> evalUnquote(shared_ptr<SExpr> sExpr, shared_ptr<Env> env) {
  shared_ptr<SExpr> unquoteArg;
  try {
    cast<NilAtom>(get(unquoteNilPos, sExpr));
    unquoteArg = cast<SExprs>(get(unquoteArgPos, sExpr))->first;
  } catch (EvalException ee) {
    handleSyntaxError(unquoteGrammar, sExpr);
  }
  return eval(unquoteArg, env);
}

shared_ptr<SExpr> expandQuasiquote(shared_ptr<SExpr> sExpr,
                                   const unsigned int level,
                                   shared_ptr<Env> env) {
  if (isa<NilAtom>(*sExpr) || isa<IntAtom>(*sExpr) || isa<BoolAtom>(*sExpr) ||
      isa<SymAtom>(*sExpr)) {
    return sExpr;
  }
  shared_ptr<SExprs> sExprs = cast<SExprs>(sExpr);
  if (level == 1) {
    if (isa<SExprs>(*sExprs->first) &&
        isa<SymAtom>(*cast<SExprs>(sExprs->first)->first) &&
        cast<SymAtom>(cast<SExprs>(sExprs->first)->first)->val ==
            "unquote-splicing") {
      shared_ptr<SExpr> res = evalUnquote(sExprs->first, env);
      if (isa<NilAtom>(*res)) {
        return expandQuasiquote(sExprs->rest, level, env);
      }
      shared_ptr<SExprs> it = cast<SExprs>(res);
      while (!isa<NilAtom>(*cast<SExprs>(it)->rest)) {
        it = cast<SExprs>(cast<SExprs>(it)->rest);
      }
      it->rest = expandQuasiquote(sExprs->rest, level, env);
      return res;
    }
    if (isa<SymAtom>(*sExprs->first) &&
        cast<SymAtom>(sExprs->first)->val == "unquote") {
      return evalUnquote(sExprs, env);
    }
  }
  if (isa<SymAtom>(*sExprs->first)) {
    string sym = cast<SymAtom>(sExprs->first)->val;
    if (sym == "unquote" || sym == "unquote-splicing") {
      return make_shared<SExprs>(
          expandQuasiquote(sExprs->first, level - 1, env),
          expandQuasiquote(sExprs->rest, level - 1, env));
    }
    if (sym == "quasiquote") {
      return make_shared<SExprs>(
          expandQuasiquote(sExprs->first, level + 1, env),
          expandQuasiquote(sExprs->rest, level + 1, env));
    }
  }
  return make_shared<SExprs>(expandQuasiquote(sExprs->first, level, env),
                             expandQuasiquote(sExprs->rest, level, env));
}

shared_ptr<SExpr> evalQuasiquote(shared_ptr<SExpr> sExpr, shared_ptr<Env> env) {
  shared_ptr<SExpr> quasiquoteArg;
  try {
    cast<NilAtom>(get(quasiquoteNilPos, sExpr));
    quasiquoteArg = cast<SExprs>(get(quasiquoteArgPos, sExpr))->first;
  } catch (EvalException ee) {
    handleSyntaxError(quasiquoteGrammar, sExpr);
  }
  return expandQuasiquote(quasiquoteArg, 1, env);
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
  shared_ptr<SExpr> res = eval(defSExpr, env);
  env->def(defSym, res);
  return res;
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
  shared_ptr<SExpr> res = eval(setSExpr, env);
  env->set(setSym, res);
  return res;
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
      if (sym == "quote") {
        return evalQuote(sExpr, env);
      } else if (sym == "quasiquote") {
        return evalQuasiquote(sExpr, env);
      } else if (sym == "unquote") {
        handleSyntaxError(unquoteGrammar, sExpr);
      } else if (sym == "unquote-splicing") {
        handleSyntaxError(unquoteSplicingGrammar, sExpr);
      } else if (sym == "define") {
        return evalDef(sExpr, env);
      } else if (sym == "set!") {
        return evalSet(sExpr, env);
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
