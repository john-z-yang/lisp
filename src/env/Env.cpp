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
      "/", make_shared<ClosureAtom>(lispDiv, env,
                                    dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(div_lhs div_rhs)"))),
                                    true)));
  env->symTable.insert(make_pair(
      "%", make_shared<ClosureAtom>(lispMod, env,
                                    dynamic_pointer_cast<SExprs>(
                                        parse(tokenize("(mod_lhs mod_rhs)"))),
                                    true)));
  env->symTable.insert(make_pair(
      "=", make_shared<ClosureAtom>(
               lispEq, env,
               dynamic_pointer_cast<SExprs>(parse(tokenize("(eq_lhs eq_rhs)"))),
               true)));
  env->symTable.insert(make_pair(
      ">", make_shared<ClosureAtom>(
               lispGt, env,
               dynamic_pointer_cast<SExprs>(parse(tokenize("(gt_lhs gt_rhs)"))),
               true)));
  env->symTable.insert(make_pair(
      ">=", make_shared<ClosureAtom>(lispGteq, env,
                                     dynamic_pointer_cast<SExprs>(parse(
                                         tokenize("(gteq_lhs gteq_rhs)"))),
                                     true)));
  env->symTable.insert(make_pair(
      "<", make_shared<ClosureAtom>(
               lispLt, env,
               dynamic_pointer_cast<SExprs>(parse(tokenize("(lt_lhs lt_rhs)"))),
               true)));
  env->symTable.insert(make_pair(
      "<=", make_shared<ClosureAtom>(lispLteq, env,
                                     dynamic_pointer_cast<SExprs>(parse(
                                         tokenize("(lteq_lhs lteq_rhs)"))),
                                     true)));
  env->symTable.insert(make_pair(
      "not", make_shared<ClosureAtom>(
                 lispNot, env,
                 dynamic_pointer_cast<SExprs>(parse(tokenize("(not_operand)"))),
                 true)));
  env->symTable.insert(make_pair(
      "and", make_shared<ClosureAtom>(lispAnd, env,
                                      dynamic_pointer_cast<SExprs>(
                                          parse(tokenize("(and_lhs and_rhs)"))),
                                      true)));
  env->symTable.insert(make_pair(
      "or", make_shared<ClosureAtom>(lispOr, env,
                                     dynamic_pointer_cast<SExprs>(
                                         parse(tokenize("(or_lhs or_rhs)"))),
                                     true)));
}