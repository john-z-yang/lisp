#ifndef LISP_SRC_ENV_FUNCTIONS_HPP_
#define LISP_SRC_ENV_FUNCTIONS_HPP_

#include "../sexpr/SExpr.hpp"
#include "./Env.hpp"
#include <memory>

std::shared_ptr<SExpr> lispQuit(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispLoad(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispDisplay(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispAbs(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispAdd(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispSub(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispMult(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispDiv(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispMod(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispEq(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispGt(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispGteq(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispLt(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispLteq(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispNot(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispAnd(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispOr(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispCons(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispCar(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispCdr(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispIsNull(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispIsCons(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispIsSym(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispIsString(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispIsNum(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispIsProc(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispIsEqv(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispGensym(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispStrLen(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispStrSub(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispStrCon(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispToStr(std::shared_ptr<Env> env);

#endif