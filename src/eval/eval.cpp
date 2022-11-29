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

std::unique_ptr<Thunk> evalCPS(std::shared_ptr<SExpr> sExpr,
                               std::shared_ptr<Env> env, EvalCont cont);

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

std::unique_ptr<Thunk> evalQuote(std::shared_ptr<SExpr> sExpr,
                                 std::shared_ptr<Env> env, EvalCont cont) {
  std::shared_ptr<SExpr> quoteArg;
  try {
    cast<NilAtom>(get(quoteNilPos, sExpr));
    quoteArg = cast<SExprs>(get(quoteArgPos, sExpr))->first;
  } catch (EvalException &ee) {
    handleSyntaxError(quoteGrammar, sExpr);
  }
  return cont(quoteArg);
}

std::unique_ptr<Thunk> evalUnquote(std::shared_ptr<SExpr> sExpr,
                                   std::shared_ptr<Env> env, EvalCont cont) {
  std::shared_ptr<SExpr> unquoteArg;
  try {
    cast<NilAtom>(get(unquoteNilPos, sExpr));
    unquoteArg = cast<SExprs>(get(unquoteArgPos, sExpr))->first;
  } catch (EvalException &ee) {
    handleSyntaxError(unquoteGrammar, sExpr);
  }
  return evalCPS(unquoteArg, env, cont);
}

std::unique_ptr<Thunk> expandQuasiquote(std::shared_ptr<SExpr> sExpr,
                                        unsigned int level,
                                        std::shared_ptr<Env> env,
                                        EvalCont cont) {
  if (isa<NilAtom>(*sExpr) || isa<IntAtom>(*sExpr) || isa<BoolAtom>(*sExpr) ||
      isa<SymAtom>(*sExpr)) {
    return cont(sExpr);
  }
  auto sExprs = cast<SExprs>(sExpr);
  if (level == 1) {
    if (isa<SExprs>(*sExprs->first) &&
        isa<SymAtom>(*cast<SExprs>(sExprs->first)->first) &&
        cast<SymAtom>(cast<SExprs>(sExprs->first)->first)->val ==
            "unquote-splicing") {
      return evalUnquote(sExprs->first, env, [=](std::shared_ptr<SExpr> res) {
        if (isa<NilAtom>(*res)) {
          return expandQuasiquote(sExprs->rest, level, env, cont);
        }
        auto it = cast<SExprs>(res);
        while (!isa<NilAtom>(*cast<SExprs>(it)->rest)) {
          it = cast<SExprs>(cast<SExprs>(it)->rest);
        }
        return expandQuasiquote(sExprs->rest, level, env,
                                [=](std::shared_ptr<SExpr> rest) {
                                  it->rest = rest;
                                  return cont(res);
                                });
      });
    }
    if (isa<SymAtom>(*sExprs->first) &&
        cast<SymAtom>(sExprs->first)->val == "unquote") {
      return evalUnquote(sExprs, env, cont);
    }
  }
  if (auto symAtom = std::dynamic_pointer_cast<SymAtom>(sExprs->first)) {
    if (symAtom->val == "unquote" || symAtom->val == "unquote-splicing") {
      level += 1;
    } else if (symAtom->val == "quasiquote") {
      level -= 1;
    }
  }
  return expandQuasiquote(
      sExprs->first, level, env, [=](std::shared_ptr<SExpr> first) {
        return expandQuasiquote(
            sExprs->rest, level, env, [=](std::shared_ptr<SExpr> rest) {
              return cont(std::make_shared<SExprs>(first, rest));
            });
      });
}

std::unique_ptr<Thunk> evalQuasiquote(std::shared_ptr<SExpr> sExpr,
                                      std::shared_ptr<Env> env, EvalCont cont) {
  std::shared_ptr<SExpr> quasiquoteArg;
  try {
    cast<NilAtom>(get(quasiquoteNilPos, sExpr));
    quasiquoteArg = cast<SExprs>(get(quasiquoteArgPos, sExpr))->first;
  } catch (EvalException &ee) {
    handleSyntaxError(quasiquoteGrammar, sExpr);
  }
  return expandQuasiquote(quasiquoteArg, 1, env, cont);
}

std::unique_ptr<Thunk> evalDef(std::shared_ptr<SExpr> sExpr,
                               std::shared_ptr<Env> env, EvalCont cont) {
  std::shared_ptr<SymAtom> sym;
  std::shared_ptr<SExpr> defSExpr;
  try {
    sym = cast<SymAtom>(cast<SExprs>(get(defSymPos, sExpr))->first);
    defSExpr = cast<SExprs>(get(defSExprPos, sExpr))->first;
    cast<NilAtom>(get(defNilPos, sExpr));
  } catch (EvalException &ee) {
    handleSyntaxError(defGrammar, sExpr);
  }
  return evalCPS(defSExpr, env, [=](std::shared_ptr<SExpr> res) {
    env->def(*sym, res);
    return cont(res);
  });
}

std::unique_ptr<Thunk> evalSet(std::shared_ptr<SExpr> sExpr,
                               std::shared_ptr<Env> env, EvalCont cont) {
  std::shared_ptr<SymAtom> sym;
  std::shared_ptr<SExpr> setSExpr;
  try {
    sym = cast<SymAtom>(cast<SExprs>(get(setSymPos, sExpr))->first);
    setSExpr = cast<SExprs>(get(setSExprPos, sExpr))->first;
    cast<NilAtom>(get(setNilPos, sExpr));
  } catch (EvalException &ee) {
    handleSyntaxError(setGrammar, sExpr);
  }
  return evalCPS(setSExpr, env, [=](std::shared_ptr<SExpr> res) {
    env->set(*sym, res);
    return cont(res);
  });
}

std::unique_ptr<Thunk> evalLambda(std::shared_ptr<SExpr> sExpr,
                                  std::shared_ptr<Env> env, const bool isMacro,
                                  EvalCont cont) {
  std::shared_ptr<SExpr> argNames;
  std::shared_ptr<SExpr> body;
  try {
    argNames = cast<SExprs>(get(lambdaArgPos, sExpr))->first;
    body = cast<SExprs>(get(lambdaBodyPos, sExpr))->first;
    cast<NilAtom>(get(lambdaNilPos, sExpr));
  } catch (EvalException &ee) {
    handleSyntaxError(lambdaGrammar, sExpr);
  }
  return cont(std::make_shared<ClosureAtom>(
      [body](std::shared_ptr<Env> _) { return body; }, env, argNames, isMacro));
}

std::unique_ptr<Thunk> evalDefMacro(std::shared_ptr<SExpr> sExpr,
                                    std::shared_ptr<Env> env, EvalCont cont) {
  std::shared_ptr<SymAtom> sym;
  std::shared_ptr<SExpr> macroExpr;
  try {
    sym = cast<SymAtom>(cast<SExprs>(get(defMacroSymPos, sExpr))->first);
    macroExpr = cast<SExprs>(get(defMacroExprPos, sExpr))->first;
    cast<NilAtom>(get(defMacroNilPos, sExpr));
  } catch (EvalException &ee) {
    handleSyntaxError(defMacroGrammar, sExpr);
  }
  return evalLambda(macroExpr, env, true, [=](std::shared_ptr<SExpr> macro) {
    env->def(*sym, macro);
    return cont(macro);
  });
}

std::unique_ptr<Thunk> evalIf(std::shared_ptr<SExpr> sExpr,
                              std::shared_ptr<Env> env, EvalCont cont) {
  std::shared_ptr<SExpr> test;
  std::shared_ptr<SExpr> conseq;
  std::shared_ptr<SExpr> alt;
  try {
    test = cast<SExprs>(get(ifTestPos, sExpr))->first;
    conseq = cast<SExprs>(get(ifConseqPos, sExpr))->first;
    alt = cast<SExprs>(get(ifAltPos, sExpr))->first;
    cast<NilAtom>(get(ifNilPos, sExpr));
  } catch (EvalException &ee) {
    handleSyntaxError(ifGrammar, sExpr);
  }
  return evalCPS(test, env, [=](std::shared_ptr<SExpr> res) {
    auto sExpr = std::make_shared<BoolAtom>(res)->val ? conseq : alt;
    return evalCPS(sExpr, env, cont);
  });
}

std::unique_ptr<Thunk> evalArgs(std::shared_ptr<SExpr> args,
                                std::shared_ptr<Env> env, EvalCont cont) {
  if (auto nilAtom = std::dynamic_pointer_cast<NilAtom>(args)) {
    return cont(nilAtom);
  }
  auto sExprs = cast<SExprs>(args);
  return evalCPS(sExprs->first, env, [=](std::shared_ptr<SExpr> first) {
    return evalArgs(sExprs->rest, env, [=](std::shared_ptr<SExpr> rest) {
      return cont(std::make_shared<SExprs>(first, rest));
    });
  });
}

std::shared_ptr<Env> loadArgs(std::shared_ptr<ClosureAtom> closure,
                              std::shared_ptr<SExpr> args,
                              std::shared_ptr<Env> curEnv) {
  auto env = std::make_shared<Env>(closure->outerEnv);
  if (isa<SExprs>(*closure->argNames)) {
    auto argNamesIter = cast<SExprs>(closure->argNames);
    auto argValsIter = cast<SExprs>(args);
    while (true) {
      env->def(*cast<SymAtom>(argNamesIter->first), argValsIter->first);
      if (isa<NilAtom>(*argNamesIter->rest) &&
          isa<NilAtom>(*argValsIter->rest)) {
        break;
      } else if (isa<NilAtom>(*argNamesIter->rest) ||
                 isa<NilAtom>(*argValsIter->rest)) {
        handleArgMismatch(closure->argNames, args);
      }
      argNamesIter = cast<SExprs>(argNamesIter->rest);
      argValsIter = cast<SExprs>(argValsIter->rest);
    }
  } else if (auto symAtom =
                 std::dynamic_pointer_cast<SymAtom>(closure->argNames)) {
    env->def(symAtom->val, args);
  } else if (!isa<NilAtom>(*args)) {
    handleArgMismatch(closure->argNames, args);
  }
  return env;
}

std::unique_ptr<Thunk> evalClosure(std::shared_ptr<ClosureAtom> closure,
                                   std::shared_ptr<SExpr> args,
                                   std::shared_ptr<Env> curEnv, EvalCont cont) {
  if (closure->isMacro) {
    auto env = loadArgs(closure, args, curEnv);
    return evalCPS(closure->proc(env), env, [=](std::shared_ptr<SExpr> body) {
      return evalCPS(body, curEnv, cont);
    });
  }
  return evalArgs(args, curEnv, [=](std::shared_ptr<SExpr> args) {
    auto env = loadArgs(closure, args, curEnv);
    return evalCPS(closure->proc(env), env, cont);
  });
}

std::unique_ptr<Thunk> evalCPS(std::shared_ptr<SExpr> sExpr,
                               std::shared_ptr<Env> env, EvalCont cont) {
  try {
    if (isa<NilAtom>(*sExpr) || isa<IntAtom>(*sExpr) || isa<BoolAtom>(*sExpr) ||
        isa<StringAtom>(*sExpr)) {
      return cont(sExpr);
    } else if (auto symAtom = std::dynamic_pointer_cast<SymAtom>(sExpr)) {
      return cont(env->find(*symAtom));
    }
    auto sExprs = cast<SExprs>(sExpr);
    if (auto sym = std::dynamic_pointer_cast<SymAtom>(sExprs->first)) {
      if (sym->val == "quote") {
        return evalQuote(sExpr, env, cont);
      } else if (sym->val == "quasiquote") {
        return evalQuasiquote(sExpr, env, cont);
      } else if (sym->val == "unquote") {
        handleSyntaxError(unquoteGrammar, sExpr);
      } else if (sym->val == "unquote-splicing") {
        handleSyntaxError(unquoteSplicingGrammar, sExpr);
      } else if (sym->val == "define") {
        return evalDef(sExpr, env, cont);
      } else if (sym->val == "set!") {
        return evalSet(sExpr, env, cont);
      } else if (sym->val == "lambda") {
        return evalLambda(sExpr, env, false, cont);
      } else if (sym->val == "define-macro") {
        return evalDefMacro(sExpr, env, cont);
      } else if (sym->val == "if") {
        return evalIf(sExpr, env, cont);
      }
    }
    return std::make_unique<Thunk>([=]() {
      return evalCPS(sExprs->first, env, [=](std::shared_ptr<SExpr> closure) {
        return evalClosure(cast<ClosureAtom>(closure), sExprs->rest, env, cont);
      });
    });
  } catch (EvalException &ee) {
    ee.pushStackTrace(sExpr);
    throw ee;
  }
}

void trampoline(std::shared_ptr<SExpr> sExpr, std::shared_ptr<Env> env,
                EvalCont cont) {
  for (auto thunk = evalCPS(sExpr, env, cont); thunk;
       thunk = thunk->execute()) {
  }
}

void eval(std::shared_ptr<SExpr> sExpr, std::shared_ptr<Env> env,
          EvalCont cont) {
  trampoline(sExpr, env, cont);
}
