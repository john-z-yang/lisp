#ifndef LISP_SRC_COMPILE_TOKEN_HPP
#define LISP_SRC_COMPILE_TOKEN_HPP

#include <ostream>
#include <string>

struct Token {
  unsigned int row;
  unsigned int col;
  std::string str;
};

std::ostream &operator<<(std::ostream &o, const Token &t);

#endif
