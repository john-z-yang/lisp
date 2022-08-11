#ifndef LISP_INCLUD_REPL_EVALEXCEPTION_H
#define LISP_INCLUD_REPL_EVALEXCEPTION_H

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
  vector<const shared_ptr<SExpr>> stackTrace;

public:
  EvalException(const string &msg);

  virtual const char *what() const noexcept override;

  void pushStackTrace(const shared_ptr<SExpr> stack);

  const vector<const shared_ptr<SExpr>> &getStackTrace() const;
};

#endif