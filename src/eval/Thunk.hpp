#ifndef LISP_SRC_EVAL_THUNK_HPP_
#define LISP_SRC_EVAL_THUNK_HPP_

#include "../env/Env.hpp"
#include "../sexpr/SExpr.hpp"
#include <functional>
#include <memory>

class Thunk {
  std::function<std::unique_ptr<Thunk>()> f;

public:
  Thunk(std::function<std::unique_ptr<Thunk>()> &&f);
  std::unique_ptr<Thunk> execute();
};

#endif
