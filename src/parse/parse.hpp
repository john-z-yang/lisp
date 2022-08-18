#ifndef LISP_SRC_PARSE_PARSE_HPP_
#define LISP_SRC_PARSE_PARSE_HPP_

#include "../sexpr/SExpr.hpp"
#include <memory>
#include <string>

using std::istream;
using std::shared_ptr;
using std::string;

shared_ptr<SExpr> parse(string str);

istream &getInput(istream &in, string &str, size_t &linesRead, string prompt,
                  string wrap);

#endif