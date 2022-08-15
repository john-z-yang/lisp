#include "EvalException.hpp"
#include <memory>
#include <ostream>
#include <string>

using std::endl;
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

ostream &operator<<(ostream &o, const EvalException &ee) {
  o << "Eval stack:" << endl;
  for (auto it : ee.getStackTrace()) {
    o << "  => " << *it << endl;
  }
  o << "Eval error: " << ee.what() << endl;
  return o;
}