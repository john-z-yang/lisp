#ifndef LISP_INCLUDE_REPL_EVAL_GRAMMAR_H_
#define LISP_INCLUDE_REPL_EVAL_GRAMMAR_H_

#include <cstdint>

const uint8_t defineNamePos = 1;
const uint8_t defineValPos = 2;

const uint8_t setNamePos = 1;
const uint8_t setValPos = 2;

const uint8_t quoteArgPos = 1;

const uint8_t ifTestPos = 1;
const uint8_t ifConseqPos = 2;
const uint8_t ifAltPos = 3;

const uint8_t lambdaArgPos = 1;
const uint8_t lambdaBodyPos = 2;

#endif