#include "eval.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/ClosureAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/StringAtom.hpp"
#include "../sexpr/SymAtom.hpp"
#include "../sexpr/cast.cpp"
#include "grammar.hpp"
#include <memory>
#include <sstream>
#include <string>

std::shared_ptr<SExpr> get(const uint8_t n, std::shared_ptr<SExpr> sExpr) {
  std::shared_ptr<SExpr> it = cast<SExprs>(sExpr);
  for (uint8_t i = 0; i < n; ++i) {
    it = cast<SExprs>(it)->rest;
  }
  return it;
}

void handleSyntaxError(std::string expected, std::shared_ptr<SExpr> actual) {
  std::stringstream ss;
  ss << "Expected \"" << expected << "\", but got \"" << *actual << "\".";
  throw EvalException(ss.str());
}

void handleArgMismatch(std::shared_ptr<SExpr> argNames,
                       std::shared_ptr<SExpr> argVals) {
  std::stringstream ss;
  ss << "Invalid number of arguments. Expected \"" << *argNames
     << "\", but got \"" << *argVals << "\".";
  throw EvalException(ss.str());
}

std::shared_ptr<SExpr> evalQuote(std::shared_ptr<SExpr> sExpr,
                                 std::shared_ptr<Env> env) {
  std::shared_ptr<SExpr> quoteArg;
  try {
    cast<NilAtom>(get(quoteNilPos, sExpr));
    quoteArg = cast<SExprs>(get(quoteArgPos, sExpr))->first;
  } catch (EvalException &ee) {
    handleSyntaxError(quoteGrammar, sExpr);
  }
  return quoteArg;
}

std::shared_ptr<SExpr> evalUnquote(std::shared_ptr<SExpr> sExpr,
                                   std::shared_ptr<Env> env) {
  std::shared_ptr<SExpr> unquoteArg;
  try {
    cast<NilAtom>(get(unquoteNilPos, sExpr));
    unquoteArg = cast<SExprs>(get(unquoteArgPos, sExpr))->first;
  } catch (EvalException &ee) {
    handleSyntaxError(unquoteGrammar, sExpr);
  }
  return eval(unquoteArg, env);
}

std::shared_ptr<SExpr> expandQuasiquote(std::shared_ptr<SExpr> sExpr,
                                        const unsigned int level,
                                        std::shared_ptr<Env> env) {
  if (isa<NilAtom>(*sExpr) || isa<IntAtom>(*sExpr) || isa<BoolAtom>(*sExpr) ||
      isa<SymAtom>(*sExpr)) {
    return sExpr;
  }
  auto sExprs = cast<SExprs>(sExpr);
  if (level == 1) {
    if (isa<SExprs>(*sExprs->first) &&
        isa<SymAtom>(*cast<SExprs>(sExprs->first)->first) &&
        cast<SymAtom>(cast<SExprs>(sExprs->first)->first)->val ==
            "unquote-splicing") {
      auto res = evalUnquote(sExprs->first, env);
      if (isa<NilAtom>(*res)) {
        return expandQuasiquote(sExprs->rest, level, env);
      }
      auto it = cast<SExprs>(res);
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
    auto sym = cast<SymAtom>(sExprs->first)->val;
    if (sym == "unquote" || sym == "unquote-splicing") {
      return std::make_shared<SExprs>(
          expandQuasiquote(sExprs->first, level - 1, env),
          expandQuasiquote(sExprs->rest, level - 1, env));
    }
    if (sym == "quasiquote") {
      return std::make_shared<SExprs>(
          expandQuasiquote(sExprs->first, level + 1, env),
          expandQuasiquote(sExprs->rest, level + 1, env));
    }
  }
  return std::make_shared<SExprs>(expandQuasiquote(sExprs->first, level, env),
                                  expandQuasiquote(sExprs->rest, level, env));
}

std::shared_ptr<SExpr> evalQuasiquote(std::shared_ptr<SExpr> sExpr,
                                      std::shared_ptr<Env> env) {
  std::shared_ptr<SExpr> quasiquoteArg;
  try {
    cast<NilAtom>(get(quasiquoteNilPos, sExpr));
    quasiquoteArg = cast<SExprs>(get(quasiquoteArgPos, sExpr))->first;
  } catch (EvalException &ee) {
    handleSyntaxError(quasiquoteGrammar, sExpr);
  }
  return expandQuasiquote(quasiquoteArg, 1, env);
}

std::shared_ptr<SExpr> evalDef(std::shared_ptr<SExpr> sExpr,
                               std::shared_ptr<Env> env) {
  std::shared_ptr<SymAtom> sym;
  std::shared_ptr<SExpr> defSExpr;
  try {
    sym = cast<SymAtom>(cast<SExprs>(get(defSymPos, sExpr))->first);
    defSExpr = cast<SExprs>(get(defSExprPos, sExpr))->first;
    cast<NilAtom>(get(defNilPos, sExpr));
  } catch (EvalException &ee) {
    handleSyntaxError(defGrammar, sExpr);
  }
  auto res = eval(defSExpr, env);
  env->def(*sym, res);
  return res;
}

std::shared_ptr<SExpr> evalSet(std::shared_ptr<SExpr> sExpr,
                               std::shared_ptr<Env> env) {
  std::shared_ptr<SymAtom> sym;
  std::shared_ptr<SExpr> setSExpr;
  try {
    sym = cast<SymAtom>(cast<SExprs>(get(setSymPos, sExpr))->first);
    setSExpr = cast<SExprs>(get(setSExprPos, sExpr))->first;
    cast<NilAtom>(get(setNilPos, sExpr));
  } catch (EvalException &ee) {
    handleSyntaxError(setGrammar, sExpr);
  }
  auto res = eval(setSExpr, env);
  env->set(*sym, res);
  return res;
}

std::shared_ptr<SExpr> evalLambda(std::shared_ptr<SExpr> sExpr,
                                  std::shared_ptr<Env> env,
                                  const bool isMacro) {
  std::shared_ptr<SExpr> argNames;
  std::shared_ptr<SExpr> body;
  try {
    argNames = cast<SExprs>(get(lambdaArgPos, sExpr))->first;
    body = cast<SExprs>(get(lambdaBodyPos, sExpr))->first;
    cast<NilAtom>(get(lambdaNilPos, sExpr));
  } catch (EvalException &ee) {
    handleSyntaxError(lambdaGrammar, sExpr);
  }
  return std::make_shared<ClosureAtom>(
      [body](std::shared_ptr<Env> env) { return body; }, env, argNames,
      isMacro);
}

std::shared_ptr<SExpr> evalDefMacro(std::shared_ptr<SExpr> sExpr,
                                    std::shared_ptr<Env> env) {
  std::shared_ptr<SymAtom> sym;
  std::shared_ptr<SExpr> macroExpr;
  try {
    sym = cast<SymAtom>(cast<SExprs>(get(defMacroSymPos, sExpr))->first);
    macroExpr = cast<SExprs>(get(defMacroExprPos, sExpr))->first;
    cast<NilAtom>(get(defMacroNilPos, sExpr));
  } catch (EvalException &ee) {
    handleSyntaxError(defMacroGrammar, sExpr);
  }
  auto macro = evalLambda(macroExpr, env, true);
  env->def(*sym, macro);
  return macro;
}

std::shared_ptr<SExpr> evalIf(std::shared_ptr<SExpr> sExpr,
                              std::shared_ptr<Env> env) {
  std::shared_ptr<BoolAtom> test;
  std::shared_ptr<SExpr> conseq;
  std::shared_ptr<SExpr> alt;
  try {
    test = std::make_shared<BoolAtom>(
        eval(cast<SExprs>(get(ifTestPos, sExpr))->first, env));
    conseq = cast<SExprs>(get(ifConseqPos, sExpr))->first;
    alt = cast<SExprs>(get(ifAltPos, sExpr))->first;
    cast<NilAtom>(get(ifNilPos, sExpr));
  } catch (EvalException &ee) {
    handleSyntaxError(ifGrammar, sExpr);
  }
  return (test->val) ? conseq : alt;
}

std::shared_ptr<SExpr> evalArgs(std::shared_ptr<SExpr> args,
                                std::shared_ptr<Env> curEnv) {
  if (isa<NilAtom>(*args)) {
    return std::make_shared<NilAtom>();
  }
  auto sExprs = cast<SExprs>(args);
  auto first = eval(sExprs->first, curEnv);
  auto rest = evalArgs(sExprs->rest, curEnv);
  return std::make_shared<SExprs>(first, rest);
}

std::shared_ptr<Env> loadArgs(std::shared_ptr<ClosureAtom> closure,
                              std::shared_ptr<SExpr> args,
                              std::shared_ptr<Env> curEnv) {
  auto env = std::make_shared<Env>(closure->outerEnv);
  auto argVals = closure->isMacro ? args : evalArgs(args, curEnv);
  if (isa<SExprs>(*closure->argNames)) {
    auto argNamesIter = cast<SExprs>(closure->argNames);
    auto argValsIter = cast<SExprs>(argVals);
    while (true) {
      env->def(*cast<SymAtom>(argNamesIter->first), argValsIter->first);
      if (isa<NilAtom>(*argNamesIter->rest) &&
          isa<NilAtom>(*argValsIter->rest)) {
        break;
      } else if (isa<NilAtom>(*argNamesIter->rest) ||
                 isa<NilAtom>(*argValsIter->rest)) {
        handleArgMismatch(closure->argNames, argVals);
      }
      argNamesIter = cast<SExprs>(argNamesIter->rest);
      argValsIter = cast<SExprs>(argValsIter->rest);
    }
  } else if (isa<SymAtom>(*closure->argNames)) {
    env->def(cast<SymAtom>(closure->argNames)->val, argVals);
  } else if (!isa<NilAtom>(*argVals)) {
    handleArgMismatch(closure->argNames, argVals);
  }
  return env;
}

std::tuple<std::shared_ptr<SExpr>, std::shared_ptr<Env>>
evalClosure(std::shared_ptr<ClosureAtom> closure, std::shared_ptr<SExpr> args,
            std::shared_ptr<Env> curEnv) {
  auto env = loadArgs(closure, args, curEnv);
  if (closure->isMacro) {
    return make_tuple(eval(closure->proc(curEnv), env), curEnv);
  }
  return make_tuple(closure->proc(env), env);
}

std::shared_ptr<SExpr> eval(std::shared_ptr<SExpr> sExpr,
                            std::shared_ptr<Env> env) {
  try {
    while (true) {
      if (isa<NilAtom>(*sExpr) || isa<IntAtom>(*sExpr) ||
          isa<BoolAtom>(*sExpr) || isa<StringAtom>(*sExpr)) {
        return sExpr;
      } else if (isa<SymAtom>(*sExpr)) {
        return env->find(*cast<SymAtom>(sExpr));
      }
      auto sExprs = cast<SExprs>(sExpr);
      if (isa<SymAtom>(*sExprs->first)) {
        auto sym = cast<SymAtom>(sExprs->first)->val;
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
        } else if (sym == "lambda") {
          return evalLambda(sExpr, env, false);
        } else if (sym == "define-macro") {
          return evalDefMacro(sExpr, env);
        } else if (sym == "if") {
          sExpr = evalIf(sExpr, env);
          continue;
        }
      }
      auto closure = cast<ClosureAtom>(eval(sExprs->first, env));
      tie(sExpr, env) = evalClosure(closure, sExprs->rest, env);
    }
  } catch (EvalException &ee) {
    ee.pushStackTrace(sExpr);
    throw ee;
  }
}
