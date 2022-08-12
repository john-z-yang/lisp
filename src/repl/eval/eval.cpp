#include "../../../include/repl/eval/eval.hpp"
#include "../../../include/repl/eval/grammar.hpp"
#include "../../../include/sexpr/BoolAtom.hpp"
#include "../../../include/sexpr/ClosureAtom.hpp"
#include "../../../include/sexpr/IntAtom.hpp"
#include "../../../include/sexpr/NilAtom.hpp"
#include "../../../include/sexpr/SExprs.hpp"
#include "../../../include/sexpr/SymAtom.hpp"
#include "../../sexpr/cast.cpp"
#include <memory>

using std::make_shared;

shared_ptr<SExpr> get(const uint8_t n, shared_ptr<SExpr> sExpr) {
  shared_ptr<SExpr> it = cast<SExprs>(sExpr);
  for (uint8_t i = 0; i < n; ++i) {
    it = cast<SExprs>(it)->rest;
  }
  return it;
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
        string name =
            cast<SymAtom>(cast<SExprs>(get(defineNamePos, sExpr))->first)->val;
        shared_ptr<SExpr> val =
            eval(cast<SExprs>(get(defineValPos, sExpr))->first, env);
        env->def(name, val);
        return val;
      } else if (sym == "set!") {
        string name =
            cast<SymAtom>(cast<SExprs>(get(setNamePos, sExpr))->first)->val;
        shared_ptr<SExpr> val =
            eval(cast<SExprs>(get(setValPos, sExpr))->first, env);
        env->set(name, val);
        return val;
      } else if (sym == "quote") {
        return cast<SExprs>(get(quoteArgPos, sExpr))->first;
      } else if (sym == "if") {
        shared_ptr<BoolAtom> test = make_shared<BoolAtom>(
            eval(cast<SExprs>(get(ifTestPos, sExpr))->first, env));
        shared_ptr<SExpr> conseq = cast<SExprs>(get(ifConseqPos, sExpr))->first;
        shared_ptr<SExpr> alt = cast<SExprs>(get(ifAltPos, sExpr))->first;
        return (test->val) ? eval(conseq, env) : eval(alt, env);
      } else if (sym == "lambda") {
        shared_ptr<SExpr> argNames =
            cast<SExprs>(get(lambdaArgPos, sExpr))->first;
        shared_ptr<SExpr> body = cast<SExprs>(get(lambdaBodyPos, sExpr))->first;
        return make_shared<ClosureAtom>(
            [body](shared_ptr<Env> env) { return eval(body, env); }, env,
            argNames);
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
