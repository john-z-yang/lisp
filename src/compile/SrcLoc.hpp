#ifndef LISP_SRC_COMPILE_SRCLOC_HPP_
#define LISP_SRC_COMPILE_SRCLOC_HPP_

namespace compile {

struct SrcLoc {
  unsigned int row;
  unsigned int col;
};

} // namespace compile

#endif
