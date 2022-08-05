#ifndef LISP_INCLUDE_ENV_FUNCTIONS_HPP_
#define LISP_INCLUDE_ENV_FUNCTIONS_HPP_

#include <iostream>
#include <memory>

#include "../sexpr/IntAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "./Env.hpp"

std::shared_ptr<SExpr> lispQuit(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispAbs(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispAdd(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispSub(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispMult(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispEq(std::shared_ptr<Env> env);

std::shared_ptr<SExpr> lispDisplay(std::shared_ptr<Env> env);

#endif