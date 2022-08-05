#include <memory>

#include "../../include/env/Env.hpp"
#include "../../include/env/functions.hpp"
#include "../../include/repl/repl.hpp"
#include "../../include/sexpr/ClosureAtom.hpp"
#include "../../include/sexpr/SExpr.hpp"
#include "../../include/sexpr/SExprs.hpp"

using std::cerr;
using std::dynamic_pointer_cast;
using std::endl;
using std::make_pair;
using std::make_shared;
using std::shared_ptr;
using std::string;

Env::Env() {}

Env::Env(const shared_ptr<Env> outer) : outer(outer) {}

shared_ptr<SExpr> Env::find(string name) {
  auto it = symTable.find(name);
  if (it != symTable.end()) {
    return it->second;
  }
  if (!outer) {
    cerr << "Fatal: symbol \"" << name << "\" is undefined" << endl;
    exit(EXIT_FAILURE);
  }
  return outer->find(name);
}

void initEnv(shared_ptr<Env> env) {
  env->symTable.insert(make_pair(
      "quit", make_shared<ClosureAtom>(lispQuit, env, nullptr, true)));
  env->symTable.insert(make_pair(
      "display", make_shared<ClosureAtom>(lispDisplay, env,
                                          dynamic_pointer_cast<SExprs>(parse(
                                              tokenize("(display_oprand)"))),
                                          true)));
  env->symTable.insert(make_pair(
      "abs", make_shared<ClosureAtom>(
                 lispAdd, env,
                 dynamic_pointer_cast<SExprs>(parse(tokenize("(abs_oprand)"))),
                 true)));
  env->symTable.insert(make_pair(
      "+", make_shared<ClosureAtom>(lispAdd, env,
                                    dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(add_lhs add_rhs)"))),
                                    true)));
  env->symTable.insert(make_pair(
      "-", make_shared<ClosureAtom>(lispSub, env,
                                    dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(sub_lhs sub_rhs)"))),
                                    true)));
  env->symTable.insert(make_pair(
      "*", make_shared<ClosureAtom>(lispMult, env,
                                    dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(mult_lhs mult_rhs)"))),
                                    true)));
  env->symTable.insert(make_pair(
      "=", make_shared<ClosureAtom>(
               lispEq, env,
               dynamic_pointer_cast<SExprs>(parse(tokenize("(eq_lhs eq_rhs)"))),
               true)));
}