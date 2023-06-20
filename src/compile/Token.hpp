#ifndef LISP_SRC_COMPILE_TOKEN_HPP_
#define LISP_SRC_COMPILE_TOKEN_HPP_

#include <string>

namespace compile {

struct Token {
  unsigned int row;
  unsigned int col;
  std::string str;
};

} // namespace compile

#endif
