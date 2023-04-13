#include "Token.hpp"

std::ostream &operator<<(std::ostream &o, const Token &t) {
  return o << "Token {" << t.str << ", " << t.lineNum << "}";
}
