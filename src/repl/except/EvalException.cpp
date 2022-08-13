#include "EvalException.hpp"
#include <memory>
#include <string>

using std::shared_ptr;
using std::string;

EvalException::EvalException(const string &msg) : _msg(msg) {}

const char *EvalException::what() const noexcept { return _msg.c_str(); }

void EvalException::pushStackTrace(shared_ptr<SExpr> stack) {
  stackTrace.push_back(stack);
}

const vector<shared_ptr<SExpr>> &EvalException::getStackTrace() const {
  return stackTrace;
}