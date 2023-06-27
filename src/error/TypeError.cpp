#include "TypeError.hpp"
#include "../sexpr/SExpr.hpp"
#include <memory>

using namespace sexpr;
using namespace error;

TypeError::TypeError(const std::string &msg, const std::string expected,
                     const SExpr &actual)
    : _msg(msg), expected(expected), actual(actual) {}

const char *TypeError::what() const noexcept { return _msg.c_str(); }

std::ostream &error::operator<<(std::ostream &o, const TypeError &te) {
  return o << te.what();
}
