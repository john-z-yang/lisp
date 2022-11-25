#ifndef LISP_SRC_EVAL_EVAL_HPP_
#define LISP_SRC_EVAL_EVAL_HPP_

#include "../env/Env.hpp"
#include "../sexpr/SExpr.hpp"
#include "Thunk.hpp"
#include <functional>
#include <memory>

typedef std::function<std::unique_ptr<Thunk>(std::shared_ptr<SExpr> sExpr)>
    EvalCont;

std::unique_ptr<Thunk> eval(std::shared_ptr<SExpr> sExpr,
                            std::shared_ptr<Env> env, EvalCont cont);

void trampoline(std::shared_ptr<SExpr> sExpr, std::shared_ptr<Env> env,
                EvalCont cont);

#endif