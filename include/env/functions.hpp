#ifndef LISP_INCLUDE_ENV_FUNCTIONS_HPP_
#define LISP_INCLUDE_ENV_FUNCTIONS_HPP_

#include <iostream>
#include <memory>

#include "../sexpr/IntAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "./Env.hpp"

using std::shared_ptr;

shared_ptr<SExpr> lispQuit(shared_ptr<Env> env);

shared_ptr<SExpr> lispDisplay(shared_ptr<Env> env);

shared_ptr<SExpr> lispAbs(shared_ptr<Env> env);

shared_ptr<SExpr> lispAdd(shared_ptr<Env> env);

shared_ptr<SExpr> lispSub(shared_ptr<Env> env);

shared_ptr<SExpr> lispMult(shared_ptr<Env> env);

shared_ptr<SExpr> lispDiv(shared_ptr<Env> env);

shared_ptr<SExpr> lispMod(shared_ptr<Env> env);

shared_ptr<SExpr> lispEq(shared_ptr<Env> env);

shared_ptr<SExpr> lispGt(shared_ptr<Env> env);

shared_ptr<SExpr> lispGteq(shared_ptr<Env> env);

shared_ptr<SExpr> lispLt(shared_ptr<Env> env);

shared_ptr<SExpr> lispLteq(shared_ptr<Env> env);

shared_ptr<SExpr> lispNot(shared_ptr<Env> env);

shared_ptr<SExpr> lispAnd(shared_ptr<Env> env);

shared_ptr<SExpr> lispOr(shared_ptr<Env> env);

shared_ptr<SExpr> lispCons(shared_ptr<Env> env);

shared_ptr<SExpr> lispCar(shared_ptr<Env> env);

shared_ptr<SExpr> lispCdr(shared_ptr<Env> env);

shared_ptr<SExpr> lispIsNull(shared_ptr<Env> env);

shared_ptr<SExpr> lispIsCons(shared_ptr<Env> env);

shared_ptr<SExpr> lispIsSym(shared_ptr<Env> env);

shared_ptr<SExpr> lispIsNum(shared_ptr<Env> env);

shared_ptr<SExpr> lispIsProc(shared_ptr<Env> env);

shared_ptr<SExpr> lispIsEqv(shared_ptr<Env> env);

#endif