#ifndef LISP_INCLUDE_ENV_ENV_HPP_
#define LISP_INCLUDE_ENV_ENV_HPP_

#include <map>

#include "../sexpr/SExpr.hpp"

using std::map;
using std::shared_ptr;
using std::string;

class Env {
public:
  map<string, shared_ptr<SExpr>> symTable;
  const shared_ptr<Env> outer;

  Env();
  Env(const shared_ptr<Env> outer);

  shared_ptr<SExpr> find(string name);
};

void initEnv(shared_ptr<Env> env);

#endif