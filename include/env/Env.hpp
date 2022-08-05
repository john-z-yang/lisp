#ifndef LISP_INCLUDE_ENV_ENV_HPP_
#define LISP_INCLUDE_ENV_ENV_HPP_

#include <map>

#include "../sexpr/SExpr.hpp"

class Env {
public:
  std::map<std::string, std::shared_ptr<SExpr>> symTable;
  const std::shared_ptr<Env> outer;

  Env();
  Env(const std::shared_ptr<Env> outer);

  std::shared_ptr<SExpr> find(std::string name);
};

void initEnv(std::shared_ptr<Env> env);

#endif