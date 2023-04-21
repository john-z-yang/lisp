#ifndef LISP_SRC_COMPILE_GRAMMAR_HPP_
#define LISP_SRC_COMPILE_GRAMMAR_HPP_

#include <cstdint>
#include <string>

const auto quoteGrammar = "(quote expression)";
const auto quoteArgPos = 1;
const auto quoteNilPos = 2;

const auto quasiquoteGrammar = "(quasiquote expression)";
const auto quasiquoteArgPos = 1;
const auto quasiquoteNilPos = 2;

const auto unquoteGrammar = "(quasiquote ... (unquote expression) ...)";
const auto unquoteArgPos = 1;
const auto unquoteNilPos = 2;

const auto unquoteSplicingGrammar =
    "(quasiquote ... (unquote-splicing (list)) ...)";
const auto unquoteSplicingArgPos = 1;
const auto unquoteSplicingNilPos = 2;

const auto defGrammar = "(define symbol expression)";
const auto defSymPos = 1;
const auto defSExprPos = 2;
const auto defNilPos = 3;

const auto setGrammar = "(set symbol expression)";
const auto setSymPos = 1;
const auto setSExprPos = 2;
const auto setNilPos = 3;

const auto initGrammer = "(symbol expression)";
const auto initSymbolPos = 0;
const auto initExprPos = 1;
const auto initNilPos = 2;

const auto lambdaGrammar =
    "(lambda (symbol*) expression) or (lambda symbol expression)";
const auto lambdaArgPos = 1;
const auto lambdaBodyPos = 2;
const auto lambdaNilPos = 3;

const auto defMacroGrammar = "(define-macro symbol (lambda ...))";
const auto defMacroSymPos = 1;
const auto defMacroExprPos = 2;
const auto defMacroNilPos = 3;

const auto ifGrammar = "(if expression_1 expression_2 expression_3)";
const auto ifTestPos = 1;
const auto ifConseqPos = 2;
const auto ifAltPos = 3;
const auto ifNilPos = 4;

#endif
