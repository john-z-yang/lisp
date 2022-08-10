#include "../../include/env/functions.hpp"
#include "../../include/sexpr/BoolAtom.hpp"
#include "../../include/sexpr/NilAtom.hpp"
#include "../../include/sexpr/SExprs.hpp"
#include <iostream>
#include <memory>

using std::cout;
using std::dynamic_pointer_cast;
using std::endl;
using std::make_shared;
using std::shared_ptr;

shared_ptr<SExpr> lispQuit(shared_ptr<Env> env) {
  cout << "Farewell." << endl;
  exit(EXIT_SUCCESS);
}

shared_ptr<SExpr> lispDisplay(shared_ptr<Env> env) {
  cout << *env->find("display_oprand") << endl;
  return make_shared<NilAtom>();
}

shared_ptr<SExpr> lispAbs(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      abs(dynamic_pointer_cast<IntAtom>(env->find("abs_oprand"))->val));
}

shared_ptr<SExpr> lispAdd(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      dynamic_pointer_cast<IntAtom>(env->find("add_lhs"))->val +
      dynamic_pointer_cast<IntAtom>(env->find("add_rhs"))->val);
}

shared_ptr<SExpr> lispSub(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      dynamic_pointer_cast<IntAtom>(env->find("sub_lhs"))->val -
      dynamic_pointer_cast<IntAtom>(env->find("sub_rhs"))->val);
}

shared_ptr<SExpr> lispMult(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      dynamic_pointer_cast<IntAtom>(env->find("mult_lhs"))->val *
      dynamic_pointer_cast<IntAtom>(env->find("mult_rhs"))->val);
}

shared_ptr<SExpr> lispDiv(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      dynamic_pointer_cast<IntAtom>(env->find("div_lhs"))->val /
      dynamic_pointer_cast<IntAtom>(env->find("div_rhs"))->val);
}

shared_ptr<SExpr> lispMod(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      dynamic_pointer_cast<IntAtom>(env->find("mod_lhs"))->val %
      dynamic_pointer_cast<IntAtom>(env->find("mod_rhs"))->val);
}

shared_ptr<SExpr> lispEq(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      (dynamic_pointer_cast<IntAtom>(env->find("eq_lhs"))->val ==
       dynamic_pointer_cast<IntAtom>(env->find("eq_rhs"))->val));
}

shared_ptr<SExpr> lispGt(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      (dynamic_pointer_cast<IntAtom>(env->find("gt_lhs"))->val >
       dynamic_pointer_cast<IntAtom>(env->find("gt_rhs"))->val));
}

shared_ptr<SExpr> lispGteq(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      (dynamic_pointer_cast<IntAtom>(env->find("gteq_lhs"))->val >=
       dynamic_pointer_cast<IntAtom>(env->find("gteq_rhs"))->val));
}

shared_ptr<SExpr> lispLt(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      (dynamic_pointer_cast<IntAtom>(env->find("lt_lhs"))->val <
       dynamic_pointer_cast<IntAtom>(env->find("lt_rhs"))->val));
}

shared_ptr<SExpr> lispLteq(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      (dynamic_pointer_cast<IntAtom>(env->find("lteq_lhs"))->val <=
       dynamic_pointer_cast<IntAtom>(env->find("lteq_rhs"))->val));
}

shared_ptr<SExpr> lispNot(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(!BoolAtom::cast(env->find("not_oprand")));
}

shared_ptr<SExpr> lispAnd(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(BoolAtom::cast(env->find("and_lhs")) &&
                               BoolAtom::cast(env->find("and_rhs")));
}

shared_ptr<SExpr> lispOr(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(BoolAtom::cast(env->find("or_lhs")) ||
                               BoolAtom::cast(env->find("or_rhs")));
}

shared_ptr<SExpr> lispCons(shared_ptr<Env> env) {
  return make_shared<SExprs>(env->find("cons_lhs"), env->find("cons_rhs"));
}

shared_ptr<SExpr> lispCar(shared_ptr<Env> env) {
  return dynamic_pointer_cast<SExprs>(env->find("car_oprand"))->first;
}

shared_ptr<SExpr> lispCdr(shared_ptr<Env> env) {
  return dynamic_pointer_cast<SExprs>(env->find("cdr_oprand"))->rest;
}

shared_ptr<SExpr> lispIsNull(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(env->find("null?_oprand")->type ==
                               SExpr::Type::NIL);
}

shared_ptr<SExpr> lispIsCons(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(env->find("cons?_oprand")->type ==
                               SExpr::Type::SEXPRS);
}

shared_ptr<SExpr> lispIsSym(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(env->find("sym?_oprand")->type ==
                               SExpr::Type::SYM);
}

shared_ptr<SExpr> lispIsNum(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(env->find("num?_oprand")->type ==
                               SExpr::Type::NUM);
}

shared_ptr<SExpr> lispIsProc(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(env->find("proc?_oprand")->type ==
                               SExpr::Type::CLOSURE);
}

shared_ptr<SExpr> lispIsEqv(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      env->find("eq?_lhs")->equals(*env->find("eq?_rhs")));
}