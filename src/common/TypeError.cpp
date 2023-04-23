#include "TypeError.hpp"
#include "sexpr/SExpr.hpp"
#include <memory>

TypeError::TypeError(const std::string &msg, const std::string expected,
                     const std::shared_ptr<SExpr> actual)
    : expected(expected), actual(actual), _msg(msg) {}

const char *TypeError::what() const noexcept { return _msg.c_str(); }

std::ostream &operator<<(std::ostream &o, const TypeError &te) {
  return o << te.what();
}
