#include "EvalException.hpp"
#include <memory>
#include <ostream>
#include <string>

EvalException::EvalException(const std::string &msg) : _msg(msg) {}

const char *EvalException::what() const noexcept { return _msg.c_str(); }

void EvalException::pushStackTrace(std::shared_ptr<SExpr> stack) {
  stackTrace.push_back(stack);
}

const std::vector<std::shared_ptr<SExpr>> &
EvalException::getStackTrace() const {
  return stackTrace;
}

std::ostream &operator<<(std::ostream &o, const EvalException &ee) {
  o << "Eval stack:" << std::endl;
  for (auto it : ee.getStackTrace()) {
    o << "  => " << *it << std::endl;
  }
  o << "Eval error: " << ee.what() << std::endl;
  return o;
}