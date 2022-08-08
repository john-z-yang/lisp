#ifndef LISP_INCLUD_REPL_REPL
#define LISP_INCLUD_REPL_REPL

#include "../env/Env.hpp"
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

shared_ptr<SExpr> eval(shared_ptr<SExpr> SExpr, shared_ptr<Env> env);

void repl();

#endif