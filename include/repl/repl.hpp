#ifndef LISP_INCLUD_REPL_REPL
#define LISP_INCLUD_REPL_REPL

#include <memory>
#include <vector>

#include "../env/Env.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"

using std::shared_ptr;
using std::string;
using std::vector;

vector<string> tokenize(string expression);

shared_ptr<SExprs> parse(vector<string>);

shared_ptr<SExprs> parse(vector<string>::iterator &it);

shared_ptr<SExpr> eval(shared_ptr<SExpr>, shared_ptr<Env>);

void repl();

#endif