#include "Env.hpp"
#include "../repl/except/EvalException.hpp"
#include "../repl/repl.hpp"
#include "../sexpr/ClosureAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/cast.cpp"
#include "functions.hpp"
#include <iostream>
#include <memory>
#include <string>

using std::cerr;
using std::endl;
using std::make_shared;
using std::shared_ptr;
using std::string;
using std::to_string;

Env::Env() {}

Env::Env(const shared_ptr<Env> outer) : outer(outer) {}

map<string, shared_ptr<SExpr>> &Env::findSymTable(string symbol) {
  if (symTable.find(symbol) != symTable.end()) {
    return symTable;
  }
  if (!outer) {
    throw EvalException("Undefined symbol \"" + symbol + "\".");
  }
  return outer->findSymTable(symbol);
}

void Env::def(string name, shared_ptr<SExpr> val) {
  auto it = symTable.find(name);
  if (it != symTable.end()) {
    throw EvalException("Symbol \"" + name + "\" is already defined.");
  }
  symTable[name] = val;
}

void Env::set(string symbol, shared_ptr<SExpr> val) {
  findSymTable(symbol)[symbol] = val;
}

shared_ptr<SExpr> Env::find(string symbol) {
  return findSymTable(symbol)[symbol];
}

void initEnv(shared_ptr<Env> env) {
  env->def("quit",
           make_shared<ClosureAtom>(lispQuit, env, make_shared<NilAtom>()));
  env->def("display", make_shared<ClosureAtom>(
                          lispDisplay, env,
                          cast<SExprs>(parse(tokenize("(display_oprand)")))));
  env->def("abs",
           make_shared<ClosureAtom>(
               lispAdd, env, cast<SExprs>(parse(tokenize("(abs_oprand)")))));
  env->def("+", make_shared<ClosureAtom>(
                    lispAdd, env,
                    cast<SExprs>(parse(tokenize("(add_lhs add_rhs)")))));
  env->def("-", make_shared<ClosureAtom>(
                    lispSub, env,
                    cast<SExprs>(parse(tokenize("(sub_lhs sub_rhs)")))));
  env->def("*", make_shared<ClosureAtom>(
                    lispMult, env,
                    cast<SExprs>(parse(tokenize("(mult_lhs mult_rhs)")))));
  env->def("/", make_shared<ClosureAtom>(
                    lispDiv, env,
                    cast<SExprs>(parse(tokenize("(div_lhs div_rhs)")))));
  env->def("%", make_shared<ClosureAtom>(
                    lispMod, env,
                    cast<SExprs>(parse(tokenize("(mod_lhs mod_rhs)")))));
  env->def("=",
           make_shared<ClosureAtom>(
               lispEq, env, cast<SExprs>(parse(tokenize("(eq_lhs eq_rhs)")))));
  env->def(">",
           make_shared<ClosureAtom>(
               lispGt, env, cast<SExprs>(parse(tokenize("(gt_lhs gt_rhs)")))));
  env->def(">=", make_shared<ClosureAtom>(
                     lispGteq, env,
                     cast<SExprs>(parse(tokenize("(gteq_lhs gteq_rhs)")))));
  env->def("<",
           make_shared<ClosureAtom>(
               lispLt, env, cast<SExprs>(parse(tokenize("(lt_lhs lt_rhs)")))));
  env->def("<=", make_shared<ClosureAtom>(
                     lispLteq, env,
                     cast<SExprs>(parse(tokenize("(lteq_lhs lteq_rhs)")))));
  env->def("not",
           make_shared<ClosureAtom>(
               lispNot, env, cast<SExprs>(parse(tokenize("(not_oprand)")))));
  env->def("and", make_shared<ClosureAtom>(
                      lispAnd, env,
                      cast<SExprs>(parse(tokenize("(and_lhs and_rhs)")))));
  env->def("or",
           make_shared<ClosureAtom>(
               lispOr, env, cast<SExprs>(parse(tokenize("(or_lhs or_rhs)")))));
  env->def("cons", make_shared<ClosureAtom>(
                       lispCons, env,
                       cast<SExprs>(parse(tokenize("(cons_lhs cons_rhs)")))));
  env->def("car",
           make_shared<ClosureAtom>(
               lispCar, env, cast<SExprs>(parse(tokenize("(car_oprand)")))));
  env->def("cdr",
           make_shared<ClosureAtom>(
               lispCdr, env, cast<SExprs>(parse(tokenize("(cdr_oprand)")))));
  env->def("null?", make_shared<ClosureAtom>(
                        lispIsNull, env,
                        cast<SExprs>(parse(tokenize("(null?_oprand)")))));

  env->def("cons?", make_shared<ClosureAtom>(
                        lispIsCons, env,
                        cast<SExprs>(parse(tokenize("(cons?_oprand)")))));

  env->def("sym?",
           make_shared<ClosureAtom>(
               lispIsSym, env, cast<SExprs>(parse(tokenize("(sym?_oprand)")))));

  env->def("num?",
           make_shared<ClosureAtom>(
               lispIsNum, env, cast<SExprs>(parse(tokenize("(num?_oprand)")))));

  env->def("proc?", make_shared<ClosureAtom>(
                        lispIsProc, env,
                        cast<SExprs>(parse(tokenize("(proc?_oprand)")))));

  env->def("eq?", make_shared<ClosureAtom>(
                      lispIsEqv, env,
                      cast<SExprs>(parse(tokenize("(eq?_lhs eq?_rhs)")))));
}