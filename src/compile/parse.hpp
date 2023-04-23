#ifndef LISP_SRC_COMPILE_PARSE_HPP_
#define LISP_SRC_COMPILE_PARSE_HPP_

#include "../common/sexpr/SExpr.hpp"
#include "SourceLoc.hpp"
#include <memory>
#include <string>
#include <vector>

void verifyLex(std::string &line, const unsigned int lineNum,
               uint32_t &openParen, uint32_t &closedParen);

std::shared_ptr<SExpr> parse(std::vector<std::string> lines,
                             SourceLoc &sourceLoc);

#endif
