#include "TypeError.hpp"
#include "SExpr.hpp"
#include <memory>

TypeError::TypeError(const std::string &msg, const std::shared_ptr<SExpr> sexpr)
    : sexpr(sexpr), _msg(msg) {}

const char *TypeError::what() const noexcept { return _msg.c_str(); }

std::ostream &operator<<(std::ostream &o, const TypeError &te) {
  return o << te.what();
}
