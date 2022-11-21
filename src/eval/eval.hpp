#ifndef LISP_SRC_EVAL_EVAL_HPP_
#define LISP_SRC_EVAL_EVAL_HPP_

#include "../env/Env.hpp"
#include "../sexpr/SExpr.hpp"
#include <memory>

std::shared_ptr<SExpr> eval(std::shared_ptr<SExpr> sExpr,
                            std::shared_ptr<Env> env);

#endif