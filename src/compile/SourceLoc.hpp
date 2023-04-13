#ifndef LISP_SRC_COMPILE_SOURCELOC_HPP
#define LISP_SRC_COMPILE_SOURCELOC_HPP

#include "../sexpr/SExpr.hpp"
#include <unordered_map>

typedef std::unordered_map<std::shared_ptr<SExpr>, unsigned int> SourceLoc;

#endif