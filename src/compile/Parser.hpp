#ifndef LISP_SRC_COMPILE_PARSER_HPP_
#define LISP_SRC_COMPILE_PARSER_HPP_

#include "../runtime/GCGuard.hpp"
#include "../runtime/VM.hpp"
#include "ParsedSrc.hpp"
#include "Token.hpp"
#include <vector>

namespace compile {

class Parser {
private:
  using TokenIter = std::vector<Token>::const_iterator;

  runtime::VM &vm;
  runtime::GCGuard gcGuard;

  const std::vector<std::string> source;
  SrcMap srcMap;
  ParsedSrc parsedSrc;

  static bool isNum(const std::string s);
  static std::vector<Token> tokenize(std::vector<std::string> lines);
  static std::vector<Token> tokenize(std::string line, const unsigned int row);

  const sexpr::SExprs *parse();
  const sexpr::SExpr *parseLists(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr *parseList(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr *parseElem(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr *parseSexprs(TokenIter &it, const TokenIter &end);
  const sexpr::SExpr *parseAtom(Token token);

public:
  static void verifyLex(
      const std::string &line,
      const unsigned int lineNum,
      unsigned int &openParen,
      unsigned int &closedParen
  );

  Parser(runtime::VM &vm, const std::vector<std::string> source);

  const ParsedSrc getParsed() const;
};

} // namespace compile

#endif
