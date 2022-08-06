#include "../../include/env/functions.hpp"
#include "../../include/sexpr/BoolAtom.hpp"
#include "../../include/sexpr/NilAtom.hpp"

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
  return make_shared<BoolAtom>(BoolAtom::cast(env->find("or_lhs")) &&
                               BoolAtom::cast(env->find("or_rhs")));
}

shared_ptr<SExpr> lispOr(shared_ptr<Env> env) {
  return make_shared<BoolAtom>(BoolAtom::cast(env->find("or_lhs")) ||
                               BoolAtom::cast(env->find("or_rhs")));
}