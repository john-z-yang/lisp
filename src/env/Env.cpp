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

Env::Env() {}

Env::Env(const std::shared_ptr<Env> outer) : outer(outer) {}

Env::SymTable &Env::findSymTable(SymAtom &sym) {
  if (symTable.find(sym) != symTable.end()) {
    return symTable;
  }
  if (!outer) {
    throw EvalException("Undefined symbol \"" + sym.val + "\".");
  }
  return outer->findSymTable(sym);
}

void Env::def(SymAtom &sym, std::shared_ptr<SExpr> val) {
  auto it = symTable.find(sym);
  if (it != symTable.end()) {
    throw EvalException("Symbol \"" + sym.val + "\" is already defined.");
  }
  symTable[sym] = val;
}

void Env::def(SymAtom &&sym, std::shared_ptr<SExpr> val) { def(sym, val); }

void Env::set(SymAtom &sym, std::shared_ptr<SExpr> val) {
  findSymTable(sym)[sym] = val;
}

std::shared_ptr<SExpr> Env::find(SymAtom &sym) {
  return findSymTable(sym)[sym];
}
std::shared_ptr<SExpr> Env::find(SymAtom &&sym) { return find(sym); }

void Env::clear() { symTable.clear(); }

void initEnv(std::shared_ptr<Env> env) {
  env->def(SymAtom("quit"), std::make_shared<ClosureAtom>(
                                lispQuit, env, std::make_shared<NilAtom>()));
  env->def(SymAtom("load"),
           std::make_shared<ClosureAtom>(lispLoad, env,
                                         cast<SExprs>(parse("(load_oprand)"))));
  env->def(SymAtom("display"),
           std::make_shared<ClosureAtom>(
               lispDisplay, env, cast<SExprs>(parse("(display_oprand)"))));
  env->def(SymAtom("abs"),
           std::make_shared<ClosureAtom>(lispAdd, env,
                                         cast<SExprs>(parse("(abs_oprand)"))));
  env->def(SymAtom("+"),
           std::make_shared<ClosureAtom>(
               lispAdd, env, cast<SExprs>(parse("(add_lhs add_rhs)"))));
  env->def(SymAtom("-"),
           std::make_shared<ClosureAtom>(
               lispSub, env, cast<SExprs>(parse("(sub_lhs sub_rhs)"))));
  env->def(SymAtom("*"),
           std::make_shared<ClosureAtom>(
               lispMult, env, cast<SExprs>(parse("(mult_lhs mult_rhs)"))));
  env->def(SymAtom("/"),
           std::make_shared<ClosureAtom>(
               lispDiv, env, cast<SExprs>(parse("(div_lhs div_rhs)"))));
  env->def(SymAtom("%"),
           std::make_shared<ClosureAtom>(
               lispMod, env, cast<SExprs>(parse("(mod_lhs mod_rhs)"))));
  env->def(SymAtom("="),
           std::make_shared<ClosureAtom>(
               lispEq, env, cast<SExprs>(parse("(eq_lhs eq_rhs)"))));
  env->def(SymAtom(">"),
           std::make_shared<ClosureAtom>(
               lispGt, env, cast<SExprs>(parse("(gt_lhs gt_rhs)"))));
  env->def(SymAtom(">="),
           std::make_shared<ClosureAtom>(
               lispGteq, env, cast<SExprs>(parse("(gteq_lhs gteq_rhs)"))));
  env->def(SymAtom("<"),
           std::make_shared<ClosureAtom>(
               lispLt, env, cast<SExprs>(parse("(lt_lhs lt_rhs)"))));
  env->def(SymAtom("<="),
           std::make_shared<ClosureAtom>(
               lispLteq, env, cast<SExprs>(parse("(lteq_lhs lteq_rhs)"))));
  env->def(SymAtom("not"),
           std::make_shared<ClosureAtom>(lispNot, env,
                                         cast<SExprs>(parse("(not_oprand)"))));
  env->def(SymAtom("cons"),
           std::make_shared<ClosureAtom>(
               lispCons, env, cast<SExprs>(parse("(cons_lhs cons_rhs)"))));
  env->def(SymAtom("car"),
           std::make_shared<ClosureAtom>(lispCar, env,
                                         cast<SExprs>(parse("(car_oprand)"))));
  env->def(SymAtom("cdr"),
           std::make_shared<ClosureAtom>(lispCdr, env,
                                         cast<SExprs>(parse("(cdr_oprand)"))));
  env->def(SymAtom("null?"),
           std::make_shared<ClosureAtom>(
               lispIsNull, env, cast<SExprs>(parse("(null?_oprand)"))));

  env->def(SymAtom("cons?"),
           std::make_shared<ClosureAtom>(
               lispIsCons, env, cast<SExprs>(parse("(cons?_oprand)"))));

  env->def(SymAtom("sym?"),
           std::make_shared<ClosureAtom>(lispIsSym, env,
                                         cast<SExprs>(parse("(sym?_oprand)"))));

  env->def(SymAtom("string?"),
           std::make_shared<ClosureAtom>(
               lispIsString, env, cast<SExprs>(parse("(string?_oprand)"))));

  env->def(SymAtom("num?"),
           std::make_shared<ClosureAtom>(lispIsNum, env,
                                         cast<SExprs>(parse("(num?_oprand)"))));

  env->def(SymAtom("proc?"),
           std::make_shared<ClosureAtom>(
               lispIsProc, env, cast<SExprs>(parse("(proc?_oprand)"))));

  env->def(SymAtom("eq?"),
           std::make_shared<ClosureAtom>(
               lispIsEqv, env, cast<SExprs>(parse("(eq?_lhs eq?_rhs)"))));

  env->def(SymAtom("gensym"),
           std::make_shared<ClosureAtom>(lispGensym, env,
                                         std::make_shared<NilAtom>()));

  env->def(SymAtom("str-len"),
           std::make_shared<ClosureAtom>(
               lispStrLen, env, cast<SExprs>(parse("(strlen_oprand)"))));

  env->def(SymAtom("str-sub"),
           std::make_shared<ClosureAtom>(
               lispStrSub, env,
               cast<SExprs>(parse("(strsub_s strsub_pos strsub_len)"))));

  env->def(
      SymAtom("str-con"),
      std::make_shared<ClosureAtom>(
          lispStrCon, env, cast<SExprs>(parse("(strcon_lhs strcon_rhs)"))));

  env->def(SymAtom("->str"),
           std::make_shared<ClosureAtom>(
               lispToStr, env, cast<SExprs>(parse("(->str_oprand)"))));

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
  (define reverse                                                             \
    (lambda (list)                                                            \
      (foldl (lambda (e v)                                                    \
               (cons e v))                                                    \
             (quote ())                                                       \
             list)))                                                          \
  "),
       env);

  eval(parse("                                                                \
  (define map                                                                 \
    (lambda (fn list)                                                         \
      (reverse                                                                \
        (foldl (lambda (e v)                                                  \
                 (cons (fn e) v))                                             \
               (quote ())                                                     \
               list))))                                                       \
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