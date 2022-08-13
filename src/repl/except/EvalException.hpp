#ifndef LISP_SRC_REPL_EXCEPT_EVALEXCEPTION_HPP_
#define LISP_SRC_REPL_EXCEPT_EVALEXCEPTION_HPP_

#include "../../sexpr/SExpr.hpp"
#include <exception>
#include <memory>
#include <string>
#include <vector>

using std::exception;
using std::shared_ptr;
using std::string;
using std::vector;

class EvalException : public exception {
  string _msg;
  vector<shared_ptr<SExpr>> stackTrace;

public:
  EvalException(const string &msg);

  virtual const char *what() const noexcept override;

  void pushStackTrace(shared_ptr<SExpr> stack);

  const vector<shared_ptr<SExpr>> &getStackTrace() const;
};

#endif