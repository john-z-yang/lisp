#include "RuntimeException.hpp"
#include <memory>
#include <ostream>
#include <string>

RuntimeException::RuntimeException(const std::string &msg) : _msg(msg) {}

const char *RuntimeException::what() const noexcept { return _msg.c_str(); }

void RuntimeException::pushStackTrace(std::shared_ptr<SExpr> stack) {
  stackTrace.push_back(stack);
}

const std::vector<std::shared_ptr<SExpr>> &
RuntimeException::getStackTrace() const {
  return stackTrace;
}

std::ostream &operator<<(std::ostream &o, const RuntimeException &ee) {
  o << "Eval stack:" << std::endl;
  for (auto it : ee.getStackTrace()) {
    o << "  => " << *it << std::endl;
  }
  o << "Eval error: " << ee.what() << std::endl;
  return o;
}
