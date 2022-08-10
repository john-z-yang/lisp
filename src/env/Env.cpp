#include "../../include/env/Env.hpp"
#include "../../include/env/functions.hpp"
#include "../../include/repl/EvalException.hpp"
#include "../../include/repl/repl.hpp"
#include "../../include/sexpr/ClosureAtom.hpp"
#include "../../include/sexpr/NilAtom.hpp"
#include "../../include/sexpr/SExpr.hpp"
#include "../../include/sexpr/SExprs.hpp"
#include <iostream>
#include <memory>
#include <string>
#include <utility>

using std::cerr;
using std::dynamic_pointer_cast;
using std::endl;
using std::make_pair;
using std::make_shared;
using std::shared_ptr;
using std::string;
using std::to_string;

Env::Env() {}

Env::Env(const shared_ptr<Env> outer) : outer(outer) {}

shared_ptr<SExpr> Env::find(string name) {
  auto it = symTable.find(name);
  if (it != symTable.end()) {
    return it->second;
  }
  if (!outer) {
    throw EvalException("Undefined symbol \"" + name + "\"");
  }
  return outer->find(name);
}

void Env::set(string name, shared_ptr<SExpr> val) {
  auto it = symTable.find(name);
  if (it != symTable.end()) {
    symTable[name] = val;
    return;
  }
  if (!outer) {
    throw EvalException("Undefined symbol \"" + name + "\"");
  }
  outer->set(name, val);
}

void initEnv(shared_ptr<Env> env) {
  env->symTable.insert(make_pair(
      "quit", make_shared<ClosureAtom>(lispQuit, env, make_shared<NilAtom>())));
  env->symTable.insert(make_pair(
      "display", make_shared<ClosureAtom>(lispDisplay, env,
                                          dynamic_pointer_cast<SExprs>(parse(
                                              tokenize("(display_oprand)"))))));
  env->symTable.insert(make_pair(
      "abs", make_shared<ClosureAtom>(lispAdd, env,
                                      dynamic_pointer_cast<SExprs>(
                                          parse(tokenize("(abs_oprand)"))))));
  env->symTable.insert(make_pair(
      "+", make_shared<ClosureAtom>(lispAdd, env,
                                    dynamic_pointer_cast<SExprs>(parse(
                                        tokenize("(add_lhs add_rhs)"))))));
  env->symTable.insert(make_pair(
      "-", make_shared<ClosureAtom>(lispSub, env,
                                    dynamic_pointer_cast<SExprs>(parse(
                                        tokenize("(sub_lhs sub_rhs)"))))));
  env->symTable.insert(make_pair(
      "*", make_shared<ClosureAtom>(lispMult, env,
                                    dynamic_pointer_cast<SExprs>(parse(
                                        tokenize("(mult_lhs mult_rhs)"))))));
  env->symTable.insert(make_pair(
      "/", make_shared<ClosureAtom>(lispDiv, env,
                                    dynamic_pointer_cast<SExprs>(parse(
                                        tokenize("(div_lhs div_rhs)"))))));
  env->symTable.insert(make_pair(
      "%", make_shared<ClosureAtom>(lispMod, env,
                                    dynamic_pointer_cast<SExprs>(parse(
                                        tokenize("(mod_lhs mod_rhs)"))))));
  env->symTable.insert(make_pair(
      "=", make_shared<ClosureAtom>(lispEq, env,
                                    dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(eq_lhs eq_rhs)"))))));
  env->symTable.insert(make_pair(
      ">", make_shared<ClosureAtom>(lispGt, env,
                                    dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(gt_lhs gt_rhs)"))))));
  env->symTable.insert(make_pair(
      ">=", make_shared<ClosureAtom>(lispGteq, env,
                                     dynamic_pointer_cast<SExprs>(parse(
                                         tokenize("(gteq_lhs gteq_rhs)"))))));
  env->symTable.insert(make_pair(
      "<", make_shared<ClosureAtom>(lispLt, env,
                                    dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(lt_lhs lt_rhs)"))))));
  env->symTable.insert(make_pair(
      "<=", make_shared<ClosureAtom>(lispLteq, env,
                                     dynamic_pointer_cast<SExprs>(parse(
                                         tokenize("(lteq_lhs lteq_rhs)"))))));
  env->symTable.insert(make_pair(
      "not", make_shared<ClosureAtom>(lispNot, env,
                                      dynamic_pointer_cast<SExprs>(
                                          parse(tokenize("(not_oprand)"))))));
  env->symTable.insert(make_pair(
      "and", make_shared<ClosureAtom>(lispAnd, env,
                                      dynamic_pointer_cast<SExprs>(parse(
                                          tokenize("(and_lhs and_rhs)"))))));
  env->symTable.insert(make_pair(
      "or", make_shared<ClosureAtom>(lispOr, env,
                                     dynamic_pointer_cast<SExprs>(
                                         parse(tokenize("(or_lhs or_rhs)"))))));
  env->symTable.insert(make_pair(
      "cons", make_shared<ClosureAtom>(lispCons, env,
                                       dynamic_pointer_cast<SExprs>(parse(
                                           tokenize("(cons_lhs cons_rhs)"))))));
  env->symTable.insert(make_pair(
      "car", make_shared<ClosureAtom>(lispCar, env,
                                      dynamic_pointer_cast<SExprs>(
                                          parse(tokenize("(car_oprand)"))))));
  env->symTable.insert(make_pair(
      "cdr", make_shared<ClosureAtom>(lispCdr, env,
                                      dynamic_pointer_cast<SExprs>(
                                          parse(tokenize("(cdr_oprand)"))))));
  env->symTable.insert(make_pair(
      "null?", make_shared<ClosureAtom>(lispIsNull, env,
                                        dynamic_pointer_cast<SExprs>(parse(
                                            tokenize("(null?_oprand)"))))));

  env->symTable.insert(make_pair(
      "cons?", make_shared<ClosureAtom>(lispIsCons, env,
                                        dynamic_pointer_cast<SExprs>(parse(
                                            tokenize("(cons?_oprand)"))))));

  env->symTable.insert(make_pair(
      "sym?", make_shared<ClosureAtom>(lispIsSym, env,
                                       dynamic_pointer_cast<SExprs>(
                                           parse(tokenize("(sym?_oprand)"))))));

  env->symTable.insert(make_pair(
      "num?", make_shared<ClosureAtom>(lispIsNum, env,
                                       dynamic_pointer_cast<SExprs>(
                                           parse(tokenize("(num?_oprand)"))))));

  env->symTable.insert(make_pair(
      "proc?", make_shared<ClosureAtom>(lispIsProc, env,
                                        dynamic_pointer_cast<SExprs>(parse(
                                            tokenize("(proc?_oprand)"))))));

  env->symTable.insert(make_pair(
      "eq?", make_shared<ClosureAtom>(lispIsEqv, env,
                                      dynamic_pointer_cast<SExprs>(parse(
                                          tokenize("(eq?_lhs eq?_rhs)"))))));
}