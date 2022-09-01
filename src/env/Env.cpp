#include "Env.hpp"
#include "../eval/EvalException.hpp"
#include "../eval/eval.hpp"
#include "../parse/parse.hpp"
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
using std::unordered_map;

Env::Env() {}

Env::Env(const shared_ptr<Env> outer) : outer(outer) {}

unordered_map<string, shared_ptr<SExpr>> &Env::findSymTable(string symbol) {
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

void Env::clear() { symTable.clear(); }

void initEnv(shared_ptr<Env> env) {
  env->def("quit",
           make_shared<ClosureAtom>(lispQuit, env, make_shared<NilAtom>()));
  env->def("display",
           make_shared<ClosureAtom>(lispDisplay, env,
                                    cast<SExprs>(parse("(display_oprand)"))));
  env->def("abs", make_shared<ClosureAtom>(
                      lispAdd, env, cast<SExprs>(parse("(abs_oprand)"))));
  env->def("+", make_shared<ClosureAtom>(
                    lispAdd, env, cast<SExprs>(parse("(add_lhs add_rhs)"))));
  env->def("-", make_shared<ClosureAtom>(
                    lispSub, env, cast<SExprs>(parse("(sub_lhs sub_rhs)"))));
  env->def("*", make_shared<ClosureAtom>(
                    lispMult, env, cast<SExprs>(parse("(mult_lhs mult_rhs)"))));
  env->def("/", make_shared<ClosureAtom>(
                    lispDiv, env, cast<SExprs>(parse("(div_lhs div_rhs)"))));
  env->def("%", make_shared<ClosureAtom>(
                    lispMod, env, cast<SExprs>(parse("(mod_lhs mod_rhs)"))));
  env->def("=", make_shared<ClosureAtom>(
                    lispEq, env, cast<SExprs>(parse("(eq_lhs eq_rhs)"))));
  env->def(">", make_shared<ClosureAtom>(
                    lispGt, env, cast<SExprs>(parse("(gt_lhs gt_rhs)"))));
  env->def(">=",
           make_shared<ClosureAtom>(
               lispGteq, env, cast<SExprs>(parse("(gteq_lhs gteq_rhs)"))));
  env->def("<", make_shared<ClosureAtom>(
                    lispLt, env, cast<SExprs>(parse("(lt_lhs lt_rhs)"))));
  env->def("<=",
           make_shared<ClosureAtom>(
               lispLteq, env, cast<SExprs>(parse("(lteq_lhs lteq_rhs)"))));
  env->def("not", make_shared<ClosureAtom>(
                      lispNot, env, cast<SExprs>(parse("(not_oprand)"))));
  env->def("cons",
           make_shared<ClosureAtom>(
               lispCons, env, cast<SExprs>(parse("(cons_lhs cons_rhs)"))));
  env->def("car", make_shared<ClosureAtom>(
                      lispCar, env, cast<SExprs>(parse("(car_oprand)"))));
  env->def("cdr", make_shared<ClosureAtom>(
                      lispCdr, env, cast<SExprs>(parse("(cdr_oprand)"))));
  env->def("null?",
           make_shared<ClosureAtom>(lispIsNull, env,
                                    cast<SExprs>(parse("(null?_oprand)"))));

  env->def("cons?",
           make_shared<ClosureAtom>(lispIsCons, env,
                                    cast<SExprs>(parse("(cons?_oprand)"))));

  env->def("sym?", make_shared<ClosureAtom>(
                       lispIsSym, env, cast<SExprs>(parse("(sym?_oprand)"))));

  env->def("num?", make_shared<ClosureAtom>(
                       lispIsNum, env, cast<SExprs>(parse("(num?_oprand)"))));

  env->def("proc?",
           make_shared<ClosureAtom>(lispIsProc, env,
                                    cast<SExprs>(parse("(proc?_oprand)"))));

  env->def("eq?",
           make_shared<ClosureAtom>(lispIsEqv, env,
                                    cast<SExprs>(parse("(eq?_lhs eq?_rhs)"))));

  env->def("gensym",
           make_shared<ClosureAtom>(lispGensym, env, make_shared<NilAtom>()));

  eval(parse("        \
  (define list        \
    (lambda lis lis)) \
  "),
       env);

  eval(parse("                          \
  (define first                         \
    (lambda (list)                      \
      (if (null? list) list             \
      (car list))))                     \
  "),
       env);

  eval(parse("                          \
  (define last                          \
    (lambda (list)                      \
      (if (null? list) list             \
      (if (null? (cdr list)) (car list) \
        (last (cdr list))))))           \
  "),
       env);

  eval(parse("               \
  (define progn              \
    (lambda lis (last lis))) \
  "),
       env);

  eval(parse("                                      \
  (define-macro and                                 \
    (lambda args                                    \
      (if (null? args) #t                           \
      (if (null? (cdr args)) (car args)             \
        `(if ,(car args) (and ,@(cdr args)) #f))))) \
  "),
       env);

  eval(parse("                                              \
  (define-macro or                                          \
    (lambda args                                            \
      (if (null? args) #f                                   \
      (if (null? (cdr args)) (car args)                     \
        `(if ,(car args) ,(car args) (or ,@(cdr args))))))) \
  "),
       env);
}