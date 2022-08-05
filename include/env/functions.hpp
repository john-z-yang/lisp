#ifndef LISP_INCLUDE_ENV_FUNCTIONS_HPP_
#define LISP_INCLUDE_ENV_FUNCTIONS_HPP_

#include <iostream>
#include <memory>

#include "../sexpr/IntAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "./Env.hpp"

using std::shared_ptr;

shared_ptr<SExpr> lispQuit(shared_ptr<Env> env);

shared_ptr<SExpr> lispAbs(shared_ptr<Env> env);

shared_ptr<SExpr> lispAdd(shared_ptr<Env> env);

shared_ptr<SExpr> lispSub(shared_ptr<Env> env);

shared_ptr<SExpr> lispMult(shared_ptr<Env> env);

shared_ptr<SExpr> lispEq(shared_ptr<Env> env);

shared_ptr<SExpr> lispDisplay(shared_ptr<Env> env);

#endif