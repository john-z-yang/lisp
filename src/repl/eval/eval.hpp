#ifndef LISP_SRC_REPL_EVAL_EVAL_HPP_
#define LISP_SRC_REPL_EVAL_EVAL_HPP_

#include "../../env/Env.hpp"
#include "../../sexpr/SExpr.hpp"
#include <memory>

using std::shared_ptr;

shared_ptr<SExpr> eval(shared_ptr<SExpr> sExpr, shared_ptr<Env> env);

#endif