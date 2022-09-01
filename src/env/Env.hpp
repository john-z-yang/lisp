#ifndef LISP_SRC_ENV_ENV_HPP_
#define LISP_SRC_ENV_ENV_HPP_

#include "../eval/EvalException.hpp"
#include "../sexpr/SExpr.hpp"
#include <memory>
#include <string>
#include <unordered_map>

using std::shared_ptr;
using std::string;
using std::unordered_map;

class Env {
private:
  unordered_map<string, shared_ptr<SExpr>> symTable;
  unordered_map<string, shared_ptr<SExpr>> &findSymTable(string symbol);

public:
  const shared_ptr<Env> outer;

  Env();
  Env(const shared_ptr<Env> outer);

  void def(string symbol, shared_ptr<SExpr> val);
  void set(string symbol, shared_ptr<SExpr> val);
  shared_ptr<SExpr> find(string symbol);
  void clear();
};

void initEnv(shared_ptr<Env> env);

#endif