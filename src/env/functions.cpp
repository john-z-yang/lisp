#include "functions.hpp"
#include "../parse/parse.hpp"
#include "../repl/repl.hpp"
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

std::shared_ptr<SExpr> lispQuit(std::shared_ptr<Env> env) {
  env->clear();
  std::cout << "Farewell." << std::endl;
  exit(EXIT_SUCCESS);
}

std::shared_ptr<SExpr> lispDisplay(std::shared_ptr<Env> env) {
  auto arg = env->find(SymAtom("display_oprand"));
  if (auto stringAtom = std::dynamic_pointer_cast<StringAtom>(arg)) {
    std::cout << stringAtom->unescaped << std::endl;
  } else {
    std::cout << *arg << std::endl;
  }
  return std::make_shared<NilAtom>();
}

std::shared_ptr<SExpr> lispLoad(std::shared_ptr<Env> env) {
  auto filePath =
      cast<StringAtom>(env->find(SymAtom("load_oprand")))->unescaped;
  if (repl(filePath, env->outer) == EXIT_SUCCESS) {
    return std::make_shared<NilAtom>();
  }
  std::stringstream ss;
  ss << "Cannot load \"" << filePath << "\"";
  throw EvalException(ss.str());
}

std::shared_ptr<SExpr> lispAbs(std::shared_ptr<Env> env) {
  return std::make_shared<IntAtom>(
      abs(cast<IntAtom>(env->find(SymAtom("abs_oprand")))->val));
}

std::shared_ptr<SExpr> lispAdd(std::shared_ptr<Env> env) {
  return std::make_shared<IntAtom>(
      cast<IntAtom>(env->find(SymAtom("add_lhs")))->val +
      cast<IntAtom>(env->find(SymAtom("add_rhs")))->val);
}

std::shared_ptr<SExpr> lispSub(std::shared_ptr<Env> env) {
  return std::make_shared<IntAtom>(
      cast<IntAtom>(env->find(SymAtom("sub_lhs")))->val -
      cast<IntAtom>(env->find(SymAtom("sub_rhs")))->val);
}

std::shared_ptr<SExpr> lispMult(std::shared_ptr<Env> env) {
  return std::make_shared<IntAtom>(
      cast<IntAtom>(env->find(SymAtom("mult_lhs")))->val *
      cast<IntAtom>(env->find(SymAtom("mult_rhs")))->val);
}

std::shared_ptr<SExpr> lispDiv(std::shared_ptr<Env> env) {
  return std::make_shared<IntAtom>(
      cast<IntAtom>(env->find(SymAtom("div_lhs")))->val /
      cast<IntAtom>(env->find(SymAtom("div_rhs")))->val);
}

std::shared_ptr<SExpr> lispMod(std::shared_ptr<Env> env) {
  return std::make_shared<IntAtom>(
      cast<IntAtom>(env->find(SymAtom("mod_lhs")))->val %
      cast<IntAtom>(env->find(SymAtom("mod_rhs")))->val);
}

std::shared_ptr<SExpr> lispEq(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      (cast<IntAtom>(env->find(SymAtom("eq_lhs")))->val ==
       cast<IntAtom>(env->find(SymAtom("eq_rhs")))->val));
}

std::shared_ptr<SExpr> lispGt(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      (cast<IntAtom>(env->find(SymAtom("gt_lhs")))->val >
       cast<IntAtom>(env->find(SymAtom("gt_rhs")))->val));
}

std::shared_ptr<SExpr> lispGteq(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      (cast<IntAtom>(env->find(SymAtom("gteq_lhs")))->val >=
       cast<IntAtom>(env->find(SymAtom("gteq_rhs")))->val));
}

std::shared_ptr<SExpr> lispLt(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      (cast<IntAtom>(env->find(SymAtom("lt_lhs")))->val <
       cast<IntAtom>(env->find(SymAtom("lt_rhs")))->val));
}

std::shared_ptr<SExpr> lispLteq(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      (cast<IntAtom>(env->find(SymAtom("lteq_lhs")))->val <=
       cast<IntAtom>(env->find(SymAtom("lteq_rhs")))->val));
}

std::shared_ptr<SExpr> lispNot(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      !BoolAtom::toBool(env->find(SymAtom("not_oprand"))));
}

std::shared_ptr<SExpr> lispCons(std::shared_ptr<Env> env) {
  auto sym = std::make_shared<SymAtom>("res");
  env->def(*sym, std::make_shared<SExprs>(env->find(SymAtom("cons_lhs")),
                                          env->find(SymAtom("cons_rhs"))));
  return sym;
}

std::shared_ptr<SExpr> lispCar(std::shared_ptr<Env> env) {
  auto sym = std::make_shared<SymAtom>("res");
  env->def(*sym, cast<SExprs>(env->find(SymAtom("car_oprand")))->first);
  return sym;
}

std::shared_ptr<SExpr> lispCdr(std::shared_ptr<Env> env) {
  auto sym = std::make_shared<SymAtom>("res");
  env->def(*sym, cast<SExprs>(env->find(SymAtom("cdr_oprand")))->rest);
  return sym;
}

std::shared_ptr<SExpr> lispIsNull(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      isa<NilAtom>(*env->find(SymAtom("null?_oprand"))));
}

std::shared_ptr<SExpr> lispIsCons(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      isa<SExprs>(*env->find(SymAtom("cons?_oprand"))));
}

std::shared_ptr<SExpr> lispIsSym(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      isa<SymAtom>(*env->find(SymAtom("sym?_oprand"))));
}

std::shared_ptr<SExpr> lispIsString(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      isa<StringAtom>(*env->find(SymAtom("string?_oprand"))));
}

std::shared_ptr<SExpr> lispIsNum(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      isa<IntAtom>(*env->find(SymAtom("num?_oprand"))));
}

std::shared_ptr<SExpr> lispIsProc(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(
      isa<ClosureAtom>(*env->find(SymAtom("proc?_oprand"))));
}

std::shared_ptr<SExpr> lispIsEqv(std::shared_ptr<Env> env) {
  return std::make_shared<BoolAtom>(*env->find(SymAtom("eq?_lhs")) ==
                                    *env->find(SymAtom("eq?_rhs")));
}

long long cnt = 0;
std::shared_ptr<SExpr> lispGensym(std::shared_ptr<Env> env) {
  std::stringstream ss;
  ss << ";GENSYM_" << cnt++;
  return std::make_shared<SExprs>(
      std::make_shared<SymAtom>("quote"),
      std::make_shared<SExprs>(std::make_shared<SymAtom>(ss.str()),
                               std::make_shared<NilAtom>()));
}

std::shared_ptr<SExpr> lispStrSub(std::shared_ptr<Env> env) {
  auto pos = cast<IntAtom>(env->find(SymAtom("strsub_pos")))->val;
  auto len = cast<IntAtom>(env->find(SymAtom("strsub_len")))->val;
  auto str = cast<StringAtom>(env->find(SymAtom("strsub_s")))->unescaped;
  std::stringstream ss;
  try {
    ss << "\"" << str.substr(pos, len) << "\"";
  } catch (std::out_of_range &ofr) {
    std::stringstream ess;
    ess << "Invalid range for "
        << cast<StringAtom>(env->find(SymAtom("strsub_s")))->literal << " ("
        << pos << ", " << len << ")";
    throw EvalException(ess.str());
  }
  return std::make_shared<StringAtom>(ss.str());
}

std::shared_ptr<SExpr> lispStrCon(std::shared_ptr<Env> env) {
  std::stringstream ss;
  ss << "\"" << cast<StringAtom>(env->find(SymAtom("strcon_lhs")))->unescaped
     << cast<StringAtom>(env->find(SymAtom("strcon_rhs")))->unescaped << "\"";
  return std::make_shared<StringAtom>(ss.str());
}

std::shared_ptr<SExpr> lispStrLen(std::shared_ptr<Env> env) {
  return std::make_shared<IntAtom>(
      cast<StringAtom>(env->find(SymAtom("strlen_oprand")))->unescaped.size());
}

std::shared_ptr<SExpr> lispToStr(std::shared_ptr<Env> env) {
  std::stringstream ss;
  ss << "\"" << *env->find(SymAtom("->str_oprand")) << "\"";
  return std::make_shared<StringAtom>(ss.str());
}