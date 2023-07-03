#ifndef LISP_SRC_COMPILE_GRAMMAR_HPP_
#define LISP_SRC_COMPILE_GRAMMAR_HPP_

#include <cstdint>
#include <string>

namespace compile {

const auto quoteGrammar = "(quote datum)";
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

const auto defMacroGrammar =
    "(defmacro name (symbol*) expression) or (defmacro name symbol expression)";
const auto defMacroSymPos = 1;
const auto defMacroArgPos = 2;
const auto defMacroBodyPos = 3;
const auto defMacroNilPos = 4;

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

const auto callGrammar = "(proc expression*)";

const auto ifGrammar = "(if expression_1 expression_2 expression_3)";
const auto ifTestPos = 1;
const auto ifConseqPos = 2;
const auto ifAltPos = 3;
const auto ifNilPos = 4;

const auto dotGrammer = "(datum* . datum)";

} // namespace compile

#endif
