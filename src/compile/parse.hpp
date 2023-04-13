#ifndef LISP_SRC_COMPILE_PARSE_HPP_
#define LISP_SRC_COMPILE_PARSE_HPP_

#include "../sexpr/SExpr.hpp"
#include <memory>
#include <string>
#include <vector>

void verifyLex(std::string &line, uint32_t &openParen, uint32_t &closedParen);

std::shared_ptr<SExpr> parse(std::vector<std::string> lines,
                             SourceLoc &sourceLoc);

#endif
