#ifndef LISP_SRC_VM_RUNTIMEEXCEPTION_HPP_
#define LISP_SRC_VM_RUNTIMEEXCEPTION_HPP_

#include "../sexpr/SExpr.hpp"
#include <exception>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

class RuntimeException : public std::exception {
  std::string _msg;
  std::vector<std::shared_ptr<SExpr>> stackTrace;

public:
  RuntimeException(const std::string &msg);

  virtual const char *what() const noexcept override;

  void pushStackTrace(std::shared_ptr<SExpr> stack);

  const std::vector<std::shared_ptr<SExpr>> &getStackTrace() const;
};

std::ostream &operator<<(std::ostream &o, const RuntimeException &ee);

#endif