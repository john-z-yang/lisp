#ifndef LISP_SRC_COMPILE_SOURCELOC_HPP
#define LISP_SRC_COMPILE_SOURCELOC_HPP

#include "../common/sexpr/SExpr.hpp"
#include <unordered_map>

typedef std::unordered_map<std::shared_ptr<SExpr>,
                           std::tuple<const unsigned int, const unsigned int>>
    SourceLoc;

#endif
