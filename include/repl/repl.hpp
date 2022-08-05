#ifndef LISP_INCLUD_REPL_REPL
#define LISP_INCLUD_REPL_REPL

#include <memory>
#include <vector>

#include "../env/Env.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"

std::vector<std::string> tokenize(std::string expression);

std::shared_ptr<SExprs> parse(std::vector<std::string>);

std::shared_ptr<SExprs> parse(std::vector<std::string>::iterator &it);

std::shared_ptr<SExpr> eval(std::shared_ptr<SExpr>, std::shared_ptr<Env>);

void repl();

#endif