#include "Token.hpp"

std::ostream &operator<<(std::ostream &o, const Token &t) { return o << t.str; }
