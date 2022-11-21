#ifndef LISP_SRC_EVAL_EVALEXCEPTION_HPP_
#define LISP_SRC_EVAL_EVALEXCEPTION_HPP_

#include "../sexpr/SExpr.hpp"
#include <exception>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

class EvalException : public std::exception {
  std::string _msg;
  std::vector<std::shared_ptr<SExpr>> stackTrace;

public:
  EvalException(const std::string &msg);

  virtual const char *what() const noexcept override;

  void pushStackTrace(std::shared_ptr<SExpr> stack);

  const std::vector<std::shared_ptr<SExpr>> &getStackTrace() const;
};

std::ostream &operator<<(std::ostream &o, const EvalException &ee);

#endif