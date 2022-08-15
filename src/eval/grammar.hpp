#ifndef LISP_SRC_EVAL_GRAMMAR_HPP_
#define LISP_SRC_EVAL_GRAMMAR_HPP_

#include <cstdint>
#include <string>

using std::string;

const string defGrammar = "(define symbol expression)";
const uint8_t defSymPos = 1;
const uint8_t defSExprPos = 2;
const uint8_t defNilPos = 3;

const string setGrammar = "(set symbol expression)";
const uint8_t setSymPos = 1;
const uint8_t setSExprPos = 2;
const uint8_t setNilPos = 3;

const string quoteGrammar = "(quote expression)";
const uint8_t quoteArgPos = 1;
const uint8_t quoteNilPos = 2;

const string ifGrammar = "(if expression_1 expression_2 expression_3)";
const uint8_t ifTestPos = 1;
const uint8_t ifConseqPos = 2;
const uint8_t ifAltPos = 3;
const uint8_t ifNilPos = 4;

const string lambdaGrammar =
    "(lambda (symbol_1 ... symbol_n) expression) or (lambda symbol expression)";
const uint8_t lambdaArgPos = 1;
const uint8_t lambdaBodyPos = 2;
const uint8_t lambdaNilPos = 3;

#endif