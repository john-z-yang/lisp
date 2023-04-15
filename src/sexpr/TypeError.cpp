#include "TypeError.hpp"

TypeError::TypeError(const std::string &msg) : _msg(msg) {}

const char *TypeError::what() const noexcept { return _msg.c_str(); }

std::ostream &operator<<(std::ostream &o, const TypeError &te) {
  return o << te.what();
}
