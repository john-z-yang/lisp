#ifndef LISP_SRC_PARSE_PARSE_HPP_
#define LISP_SRC_PARSE_PARSE_HPP_

#include "../sexpr/SExpr.hpp"
#include <memory>
#include <string>

std::shared_ptr<SExpr> parse(std::string str);

std::istream &getInput(std::istream &in, std::string &str,
                       std::size_t &linesRead, std::string prompt,
                       std::string wrap);

#endif