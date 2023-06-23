#ifndef LISP_SRC_COMPILE_TOKEN_HPP_
#define LISP_SRC_COMPILE_TOKEN_HPP_

#include "SrcLoc.hpp"
#include <string>

namespace compile {

struct Token {
  std::string str;
  SrcLoc srcLoc;
};

} // namespace compile

#endif
