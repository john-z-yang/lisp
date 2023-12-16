#ifndef LISP_SRC_COMPILE_SRCMAP_HPP_
#define LISP_SRC_COMPILE_SRCMAP_HPP_

#include "../sexpr/SExpr.hpp"
#include "SrcLoc.hpp"
#include <unordered_map>

namespace compile {

using SrcMap = std::unordered_map<const sexpr::SExpr *, SrcLoc>;

}

#endif
