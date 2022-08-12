#ifndef LISP_INCLUDE_REPL_EVAL_EVAL_H
#define LISP_INCLUDE_REPL_EVAL_EVAL_H

#include "../../env/Env.hpp"
#include "../../sexpr/SExpr.hpp"
#include <memory>

using std::shared_ptr;

shared_ptr<SExpr> eval(shared_ptr<SExpr> sExpr, shared_ptr<Env> env);

#endif