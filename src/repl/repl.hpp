#ifndef LISP_SRC_REPL_REPL_HPP_
#define LISP_SRC_REPL_REPL_HPP_

#include "../env/Env.hpp"
#include "../repl/except/EvalException.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include <memory>
#include <string>
#include <vector>

using std::shared_ptr;
using std::string;
using std::vector;

vector<string> tokenize(string expression);

shared_ptr<SExpr> parseAtom(string token);

shared_ptr<SExpr> parse(vector<string>::iterator &it);

shared_ptr<SExpr> parse(vector<string> tokens);

void repl();

#endif