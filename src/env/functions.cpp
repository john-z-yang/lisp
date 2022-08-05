#include "../../include/env/functions.hpp"
#include "../../include/sexpr/BoolAtom.hpp"

std::shared_ptr<SExpr> lispQuit(std::shared_ptr<Env> env) {
  std::cout << "Farewell." << std::endl;
  exit(EXIT_SUCCESS);
}

std::shared_ptr<SExpr> lispAbs(std::shared_ptr<Env> env) {
  return std::make_shared<IntAtom>(
      abs(std::dynamic_pointer_cast<IntAtom>(env->find("abs_oprand"))->val));
}

std::shared_ptr<SExpr> lispAdd(std::shared_ptr<Env> env) {
  return std::make_shared<IntAtom>(
      std::dynamic_pointer_cast<IntAtom>(env->find("add_lhs"))->val +
      std::dynamic_pointer_cast<IntAtom>(env->find("add_rhs"))->val);
}

std::shared_ptr<SExpr> lispSub(std::shared_ptr<Env> env) {
  return std::make_shared<IntAtom>(
      std::dynamic_pointer_cast<IntAtom>(env->find("sub_lhs"))->val -
      std::dynamic_pointer_cast<IntAtom>(env->find("sub_rhs"))->val);
}

std::shared_ptr<SExpr> lispMult(std::shared_ptr<Env> env) {
  return std::make_shared<IntAtom>(
      std::dynamic_pointer_cast<IntAtom>(env->find("mult_lhs"))->val *
      std::dynamic_pointer_cast<IntAtom>(env->find("mult_rhs"))->val);
}

std::shared_ptr<SExpr> lispEq(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      (std::dynamic_pointer_cast<IntAtom>(env->find("eq_lhs"))->val ==
       std::dynamic_pointer_cast<IntAtom>(env->find("eq_rhs"))->val)
          ? true
          : false);
}

std::shared_ptr<SExpr> lispDisplay(std::shared_ptr<Env> env) {
  std::cout << *env->find("display_oprand") << std::endl;
  return nullptr;
}