#ifndef LISP_SRC_COMPILE_PARSEDSRC_HPP_
#define LISP_SRC_COMPILE_PARSEDSRC_HPP_

#include "../sexpr/SExprs.hpp"
#include "SrcMap.hpp"
#include <vector>

namespace compile {

struct ParsedSrc {
  std::vector<std::string> source;
  SrcMap srcMap;
  const sexpr::SExprs *root;
};

} // namespace compile

#endif
