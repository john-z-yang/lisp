#ifndef LISP_INCLUDE_ENV_ENV_HPP_
#define LISP_INCLUDE_ENV_ENV_HPP_

#include "../repl/EvalException.hpp"
#include "../sexpr/SExpr.hpp"
#include <map>
#include <memory>
#include <string>

using std::map;
using std::shared_ptr;
using std::string;

class Env {
public:
  map<string, shared_ptr<SExpr>> symTable;
  const shared_ptr<Env> outer;

  Env();
  Env(const shared_ptr<Env> outer);

  shared_ptr<SExpr> find(string symbol);

  void set(string symbol, shared_ptr<SExpr> val);
};

void initEnv(shared_ptr<Env> env);

#endif