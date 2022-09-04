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

Env::SymTable &Env::findSymTable(SymAtom &sym) {
  if (symTable.find(sym) != symTable.end()) {
    return symTable;
  }
  if (!outer) {
    throw EvalException("Undefined symbol \"" + sym.val + "\".");
  }
  return outer->findSymTable(sym);
}

void Env::def(SymAtom &sym, shared_ptr<SExpr> val) {
  auto it = symTable.find(sym);
  if (it != symTable.end()) {
    throw EvalException("Symbol \"" + sym.val + "\" is already defined.");
  }
  symTable[sym] = val;
}

void Env::def(SymAtom &&sym, shared_ptr<SExpr> val) { def(sym, val); }

void Env::set(SymAtom &sym, shared_ptr<SExpr> val) {
  findSymTable(sym)[sym] = val;
}

shared_ptr<SExpr> Env::find(SymAtom &sym) { return findSymTable(sym)[sym]; }
shared_ptr<SExpr> Env::find(SymAtom &&sym) { return find(sym); }

void Env::clear() { symTable.clear(); }

void initEnv(shared_ptr<Env> env) {
  env->def(SymAtom("quit"),
           make_shared<ClosureAtom>(lispQuit, env, make_shared<NilAtom>()));
  env->def(SymAtom("display"),
           make_shared<ClosureAtom>(lispDisplay, env,
                                    cast<SExprs>(parse("(display_oprand)"))));
  env->def(SymAtom("abs"),
           make_shared<ClosureAtom>(lispAdd, env,
                                    cast<SExprs>(parse("(abs_oprand)"))));
  env->def(SymAtom("+"),
           make_shared<ClosureAtom>(lispAdd, env,
                                    cast<SExprs>(parse("(add_lhs add_rhs)"))));
  env->def(SymAtom("-"),
           make_shared<ClosureAtom>(lispSub, env,
                                    cast<SExprs>(parse("(sub_lhs sub_rhs)"))));
  env->def(SymAtom("*"),
           make_shared<ClosureAtom>(
               lispMult, env, cast<SExprs>(parse("(mult_lhs mult_rhs)"))));
  env->def(SymAtom("/"),
           make_shared<ClosureAtom>(lispDiv, env,
                                    cast<SExprs>(parse("(div_lhs div_rhs)"))));
  env->def(SymAtom("%"),
           make_shared<ClosureAtom>(lispMod, env,
                                    cast<SExprs>(parse("(mod_lhs mod_rhs)"))));
  env->def(SymAtom("="),
           make_shared<ClosureAtom>(lispEq, env,
                                    cast<SExprs>(parse("(eq_lhs eq_rhs)"))));
  env->def(SymAtom(">"),
           make_shared<ClosureAtom>(lispGt, env,
                                    cast<SExprs>(parse("(gt_lhs gt_rhs)"))));
  env->def(SymAtom(">="),
           make_shared<ClosureAtom>(
               lispGteq, env, cast<SExprs>(parse("(gteq_lhs gteq_rhs)"))));
  env->def(SymAtom("<"),
           make_shared<ClosureAtom>(lispLt, env,
                                    cast<SExprs>(parse("(lt_lhs lt_rhs)"))));
  env->def(SymAtom("<="),
           make_shared<ClosureAtom>(
               lispLteq, env, cast<SExprs>(parse("(lteq_lhs lteq_rhs)"))));
  env->def(SymAtom("not"),
           make_shared<ClosureAtom>(lispNot, env,
                                    cast<SExprs>(parse("(not_oprand)"))));
  env->def(SymAtom("cons"),
           make_shared<ClosureAtom>(
               lispCons, env, cast<SExprs>(parse("(cons_lhs cons_rhs)"))));
  env->def(SymAtom("car"),
           make_shared<ClosureAtom>(lispCar, env,
                                    cast<SExprs>(parse("(car_oprand)"))));
  env->def(SymAtom("cdr"),
           make_shared<ClosureAtom>(lispCdr, env,
                                    cast<SExprs>(parse("(cdr_oprand)"))));
  env->def(SymAtom("null?"),
           make_shared<ClosureAtom>(lispIsNull, env,
                                    cast<SExprs>(parse("(null?_oprand)"))));

  env->def(SymAtom("cons?"),
           make_shared<ClosureAtom>(lispIsCons, env,
                                    cast<SExprs>(parse("(cons?_oprand)"))));

  env->def(SymAtom("sym?"),
           make_shared<ClosureAtom>(lispIsSym, env,
                                    cast<SExprs>(parse("(sym?_oprand)"))));

  env->def(SymAtom("num?"),
           make_shared<ClosureAtom>(lispIsNum, env,
                                    cast<SExprs>(parse("(num?_oprand)"))));

  env->def(SymAtom("proc?"),
           make_shared<ClosureAtom>(lispIsProc, env,
                                    cast<SExprs>(parse("(proc?_oprand)"))));

  env->def(SymAtom("eq?"),
           make_shared<ClosureAtom>(lispIsEqv, env,
                                    cast<SExprs>(parse("(eq?_lhs eq?_rhs)"))));

  env->def(SymAtom("gensym"),
           make_shared<ClosureAtom>(lispGensym, env, make_shared<NilAtom>()));

  eval(parse("                                                                \
  (define list                                                                \
    (lambda lis lis))                                                         \
  "),
       env);

  eval(parse("                                                                \
  (define first                                                               \
    (lambda (list)                                                            \
      (if (null? list)                                                        \
        list                                                                  \
      (car list))))                                                           \
  "),
       env);

  eval(parse("                                                                \
  (define last                                                                \
    (lambda (list)                                                            \
      (if (null? list)                                                        \
        list                                                                  \
      (if (null? (cdr list))                                                  \
        (car list)                                                            \
      (last (cdr list))))))                                                   \
  "),
       env);

  eval(parse("                                                                \
  (define foldl                                                               \
    (lambda (fn cur list)                                                     \
      (if (null? list)                                                        \
        cur                                                                   \
      (foldl fn                                                               \
             (fn (car list) cur)                                              \
             (cdr list)))))                                                   \
  "),
       env);

  eval(parse("                                                                \
  (define map                                                                 \
    (lambda (fn lis)                                                          \
      (if (null? lis)                                                         \
        (quote ())                                                            \
      (cons (fn (car lis))                                                    \
            (map fn (cdr lis))))))                                            \
  "),
       env);

  eval(parse("                                                                \
  (define progn                                                               \
    (lambda lis (last lis)))                                                  \
  "),
       env);

  eval(parse("                                                                \
  (define-macro and                                                           \
    (lambda args                                                              \
      (if (null? args)                                                        \
        #t                                                                    \
      (if (null? (cdr args))                                                  \
        (car args)                                                            \
      `(if ,(car args) (and ,@(cdr args)) #f)))))                             \
  "),
       env);

  eval(parse("                                                                \
  (define-macro or                                                            \
    (lambda args                                                              \
      (if (null? args)                                                        \
        #f                                                                    \
      (if (null? (cdr args))                                                  \
        (car args)                                                            \
      `(if ,(car args) ,(car args) (or ,@(cdr args)))))))                     \
  "),
       env);
}