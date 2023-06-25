#ifndef LISP_SRC_ERROR_RUNTIMEERROR_HPP_
#define LISP_SRC_ERROR_RUNTIMEERROR_HPP_

#include "../runtime/CallFrame.hpp"
#include "../runtime/Env.hpp"
#include <exception>
#include <ostream>

namespace error {

class RuntimeError : public std::exception {
  friend std::ostream &operator<<(std::ostream &o, const RuntimeError &re);

private:
  std::string _msg;
  const runtime::Env globals;
  const std::vector<std::reference_wrapper<const sexpr::SExpr>> stack;
  const std::vector<runtime::CallFrame> frames;

public:
  RuntimeError(const std::string &msg, runtime::Env globals,
               std::vector<std::reference_wrapper<const sexpr::SExpr>> stack,
               std::vector<runtime::CallFrame> frames);

  virtual const char *what() const noexcept override;
};

std::ostream &operator<<(std::ostream &o, const RuntimeError &re);

} // namespace error

#endif
