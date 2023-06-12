#ifndef LISP_SRC_VM_RUNTIMEERROR_HPP_
#define LISP_SRC_VM_RUNTIMEERROR_HPP_

#include "Env.hpp"
#include "VM.hpp"
#include <exception>
#include <ostream>

class RuntimeError : public std::exception {
  friend std::ostream &operator<<(std::ostream &o, const RuntimeError &re);

private:
  std::string _msg;
  const Env globals;
  const std::vector<const SExpr *> stack;
  const std::vector<VM::CallFrame> frames;

public:
  RuntimeError(const std::string &msg, Env globals,
               std::vector<const SExpr *> stack,
               std::vector<VM::CallFrame> frames);

  virtual const char *what() const noexcept override;
};

std::ostream &operator<<(std::ostream &o, const RuntimeError &re);

#endif
