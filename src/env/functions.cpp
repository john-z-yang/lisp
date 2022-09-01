#include "functions.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/ClosureAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/SymAtom.hpp"
#include "../sexpr/cast.cpp"
#include <iostream>
#include <memory>

using std::cout;
using std::endl;
using std::make_shared;
using std::shared_ptr;

shared_ptr<SExpr> lispQuit(shared_ptr<Env> env) {
  env->clear();
  cout << "Farewell." << endl;
  exit(EXIT_SUCCESS);
}

shared_ptr<SExpr> lispDisplay(shared_ptr<Env> env) {
  cout << *env->find("display_oprand") << endl;
  return make_shared<NilAtom>();
}

shared_ptr<SExpr> lispAbs(shared_ptr<Env> env) {
  return make_shared<IntAtom>(abs(cast<IntAtom>(env->find("abs_oprand"))->val));
}

shared_ptr<SExpr> lispAdd(shared_ptr<Env> env) {
  return make_shared<IntAtom>(cast<IntAtom>(env->find("add_lhs"))->val +
                              cast<IntAtom>(env->find("add_rhs"))->val);
}

shared_ptr<SExpr> lispSub(shared_ptr<Env> env) {
  return make_shared<IntAtom>(cast<IntAtom>(env->find("sub_lhs"))->val -
                              cast<IntAtom>(env->find("sub_rhs"))->val);
}

shared_ptr<SExpr> lispMult(shared_ptr<Env> env) {
  return make_shared<IntAtom>(cast<IntAtom>(env->find("mult_lhs"))->val *
                              cast<IntAtom>(env->find("mult_rhs"))->val);
}

shared_ptr<SExpr> lispDiv(shared_ptr<Env> env) {
  return make_shared<IntAtom>(cast<IntAtom>(env->find("div_lhs"))->val /
                              cast<IntAtom>(env->find("div_rhs"))->val);
}

shared_ptr<SExpr> lispMod(shared_ptr<Env> env) {
  return make_shared<IntAtom>(cast<IntAtom>(env->find("mod_lhs"))->val %
                              cast<IntAtom>(env->find("mod_rhs"))->val);
}

shared_ptr<SExpr> lispEq(shared_ptr<Env> env) {
  return make_shared<BoolAtom>((cast<IntAtom>(env->find("eq_lhs"))->val ==
                                cast<IntAtom>(env->find("eq_rhs"))->val));
}

shared_ptr<SExpr> lispGt(shared_ptr<Env> env) {
  return make_shared<BoolAtom>((cast<IntAtom>(env->find("gt_lhs"))->val >
                                cast<IntAtom>(env->find("gt_rhs"))->val));
}

shared_ptr<SExpr> lispGteq(shared_ptr<Env> env) {
  return make_shared<BoolAtom>((cast<IntAtom>(env->find("gteq_lhs"))->val >=
                                cast<IntAtom>(env->find("gteq_rhs"))->val));
}

shared_ptr<SExpr> lispLt(shared_ptr<Env> env) {
  return make_shared<BoolAtom>((cast<IntAtom>(env->find("lt_lhs"))->val <
                                cast<IntAtom>(env->find("lt_rhs"))->val));
}

shared_ptr<SExpr> lispLteq(shared_ptr<Env> env) {
  return make_shared<BoolAtom>((cast<IntAtom>(env->find("lteq_lhs"))->val <=
                                cast<IntAtom>(env->find("lteq_rhs"))->val));
}

shared_ptr<SExpr> lispNot(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(!BoolAtom::toBool(env->find("not_oprand")));
}

shared_ptr<SExpr> lispCons(shared_ptr<Env> env) {
  return make_shared<SExprs>(env->find("cons_lhs"), env->find("cons_rhs"));
}

shared_ptr<SExpr> lispCar(shared_ptr<Env> env) {
  return cast<SExprs>(env->find("car_oprand"))->first;
}

shared_ptr<SExpr> lispCdr(shared_ptr<Env> env) {
  return cast<SExprs>(env->find("cdr_oprand"))->rest;
}

shared_ptr<SExpr> lispIsNull(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(isa<NilAtom>(*env->find("null?_oprand")));
}

shared_ptr<SExpr> lispIsCons(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(isa<SExprs>(*env->find("cons?_oprand")));
}

shared_ptr<SExpr> lispIsSym(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(isa<SymAtom>(*env->find("sym?_oprand")));
}

shared_ptr<SExpr> lispIsNum(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(isa<IntAtom>(*env->find("num?_oprand")));
}

shared_ptr<SExpr> lispIsProc(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(isa<ClosureAtom>(*env->find("proc?_oprand")));
}

shared_ptr<SExpr> lispIsEqv(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(*env->find("eq?_lhs") == *env->find("eq?_rhs"));
}

long long cnt = 0;
shared_ptr<SExpr> lispGensym(shared_ptr<Env> env) {
  stringstream ss;
  ss << "(_GENSYM_)_#" << cnt++;
  return make_shared<SymAtom>(ss.str());
}