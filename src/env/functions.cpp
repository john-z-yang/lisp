#include "functions.hpp"
#include "../parse/parse.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/ClosureAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/StringAtom.hpp"
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
  shared_ptr<SExpr> arg = env->find(SymAtom("display_oprand"));
  if (isa<StringAtom>(*arg)) {
    cout << cast<StringAtom>(arg)->unescaped << endl;
  } else {
    cout << *arg << endl;
  }
  return make_shared<NilAtom>();
}

shared_ptr<SExpr> lispAbs(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      abs(cast<IntAtom>(env->find(SymAtom("abs_oprand")))->val));
}

shared_ptr<SExpr> lispAdd(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      cast<IntAtom>(env->find(SymAtom("add_lhs")))->val +
      cast<IntAtom>(env->find(SymAtom("add_rhs")))->val);
}

shared_ptr<SExpr> lispSub(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      cast<IntAtom>(env->find(SymAtom("sub_lhs")))->val -
      cast<IntAtom>(env->find(SymAtom("sub_rhs")))->val);
}

shared_ptr<SExpr> lispMult(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      cast<IntAtom>(env->find(SymAtom("mult_lhs")))->val *
      cast<IntAtom>(env->find(SymAtom("mult_rhs")))->val);
}

shared_ptr<SExpr> lispDiv(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      cast<IntAtom>(env->find(SymAtom("div_lhs")))->val /
      cast<IntAtom>(env->find(SymAtom("div_rhs")))->val);
}

shared_ptr<SExpr> lispMod(shared_ptr<Env> env) {
  return make_shared<IntAtom>(
      cast<IntAtom>(env->find(SymAtom("mod_lhs")))->val %
      cast<IntAtom>(env->find(SymAtom("mod_rhs")))->val);
}

shared_ptr<SExpr> lispEq(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      (cast<IntAtom>(env->find(SymAtom("eq_lhs")))->val ==
       cast<IntAtom>(env->find(SymAtom("eq_rhs")))->val));
}

shared_ptr<SExpr> lispGt(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      (cast<IntAtom>(env->find(SymAtom("gt_lhs")))->val >
       cast<IntAtom>(env->find(SymAtom("gt_rhs")))->val));
}

shared_ptr<SExpr> lispGteq(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      (cast<IntAtom>(env->find(SymAtom("gteq_lhs")))->val >=
       cast<IntAtom>(env->find(SymAtom("gteq_rhs")))->val));
}

shared_ptr<SExpr> lispLt(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      (cast<IntAtom>(env->find(SymAtom("lt_lhs")))->val <
       cast<IntAtom>(env->find(SymAtom("lt_rhs")))->val));
}

shared_ptr<SExpr> lispLteq(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      (cast<IntAtom>(env->find(SymAtom("lteq_lhs")))->val <=
       cast<IntAtom>(env->find(SymAtom("lteq_rhs")))->val));
}

shared_ptr<SExpr> lispNot(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      !BoolAtom::toBool(env->find(SymAtom("not_oprand"))));
}

shared_ptr<SExpr> lispCons(shared_ptr<Env> env) {
  shared_ptr<SymAtom> sym = make_shared<SymAtom>("res");
  env->def(*sym, make_shared<SExprs>(env->find(SymAtom("cons_lhs")),
                                     env->find(SymAtom("cons_rhs"))));
  return sym;
}

shared_ptr<SExpr> lispCar(shared_ptr<Env> env) {
  shared_ptr<SymAtom> sym = make_shared<SymAtom>("res");
  env->def(*sym, cast<SExprs>(env->find(SymAtom("car_oprand")))->first);
  return sym;
}

shared_ptr<SExpr> lispCdr(shared_ptr<Env> env) {
  shared_ptr<SymAtom> sym = make_shared<SymAtom>("res");
  env->def(*sym, cast<SExprs>(env->find(SymAtom("cdr_oprand")))->rest);
  return sym;
}

shared_ptr<SExpr> lispIsNull(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      isa<NilAtom>(*env->find(SymAtom("null?_oprand"))));
}

shared_ptr<SExpr> lispIsCons(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      isa<SExprs>(*env->find(SymAtom("cons?_oprand"))));
}

shared_ptr<SExpr> lispIsSym(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      isa<SymAtom>(*env->find(SymAtom("sym?_oprand"))));
}

shared_ptr<SExpr> lispIsString(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      isa<StringAtom>(*env->find(SymAtom("string?_oprand"))));
}

shared_ptr<SExpr> lispIsNum(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      isa<IntAtom>(*env->find(SymAtom("num?_oprand"))));
}

shared_ptr<SExpr> lispIsProc(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(
      isa<ClosureAtom>(*env->find(SymAtom("proc?_oprand"))));
}

shared_ptr<SExpr> lispIsEqv(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(*env->find(SymAtom("eq?_lhs")) ==
                               *env->find(SymAtom("eq?_rhs")));
}

long long cnt = 0;
shared_ptr<SExpr> lispGensym(shared_ptr<Env> env) {
  stringstream ss;
  ss << ";GENSYM_" << cnt++;
  return make_shared<SExprs>(make_shared<SymAtom>("quote"),
                             make_shared<SExprs>(make_shared<SymAtom>(ss.str()),
                                                 make_shared<NilAtom>()));
}