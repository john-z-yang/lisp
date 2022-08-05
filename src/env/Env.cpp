#include <memory>

#include "../../include/env/Env.hpp"
#include "../../include/env/functions.hpp"
#include "../../include/repl/repl.hpp"
#include "../../include/sexpr/ClosureAtom.hpp"
#include "../../include/sexpr/SExpr.hpp"
#include "../../include/sexpr/SExprs.hpp"

Env::Env() {}

Env::Env(const std::shared_ptr<Env> outer) : outer(outer) {}

std::shared_ptr<SExpr> Env::find(std::string name) {
  auto it = symTable.find(name);
  if (it != symTable.end()) {
    return it->second;
  }
  if (!outer) {
    std::cerr << "Fatal: symbol \"" << name << "\" is undefined" << std::endl;
    exit(EXIT_FAILURE);
  }
  return outer->find(name);
}

void initEnv(std::shared_ptr<Env> env) {
  env->symTable.insert(std::make_pair(
      "quit", std::make_shared<ClosureAtom>(lispQuit, env, nullptr, true)));
  env->symTable.insert(std::make_pair(
      "display",
      std::make_shared<ClosureAtom>(lispDisplay, env,
                                    std::dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(display_oprand)"))),
                                    true)));
  env->symTable.insert(std::make_pair(
      "abs", std::make_shared<ClosureAtom>(lispAdd, env,
                                           std::dynamic_pointer_cast<SExprs>(
                                               parse(tokenize("(abs_oprand)"))),
                                           true)));
  env->symTable.insert(std::make_pair(
      "+",
      std::make_shared<ClosureAtom>(lispAdd, env,
                                    std::dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(add_lhs add_rhs)"))),
                                    true)));
  env->symTable.insert(std::make_pair(
      "-",
      std::make_shared<ClosureAtom>(lispSub, env,
                                    std::dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(sub_lhs sub_rhs)"))),
                                    true)));
  env->symTable.insert(std::make_pair(
      "*",
      std::make_shared<ClosureAtom>(lispMult, env,
                                    std::dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(mult_lhs mult_rhs)"))),
                                    true)));
  env->symTable.insert(std::make_pair(
      "=",
      std::make_shared<ClosureAtom>(
          lispEq, env,
          std::dynamic_pointer_cast<SExprs>(parse(tokenize("(eq_lhs eq_rhs)"))),
          true)));
}