#include "../common/sexpr/BoolAtom.hpp"
#include "../common/sexpr/IntAtom.hpp"
#include "../common/sexpr/NilAtom.hpp"
#include "../common/sexpr/SExpr.hpp"
#include "../common/sexpr/SExprs.hpp"
#include "../common/sexpr/StringAtom.hpp"
#include "../common/sexpr/SymAtom.hpp"
#include "SourceLoc.hpp"
#include "SyntaxError.hpp"
#include "Token.hpp"
#include <algorithm>
#include <iostream>
#include <iterator>
#include <memory>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

std::vector<Token> tokenize(std::string line, const unsigned int row) {
  std::vector<Token> tokens;
  std::regex rgx(
      "\\\"(?:[^\"\\\\]*(?:\\\\.)?)*\\\"|;|\\(|\\)|,@|,|`|'|[^\\s(),@,`']+");
  auto begin = std::sregex_iterator(line.begin(), line.end(), rgx);
  auto end = std::sregex_iterator();
  for (std::sregex_iterator i = begin; i != end; ++i) {
    std::smatch match = *i;
    tokens.push_back({row, (unsigned int)match.position(), match.str()});
  }
  return tokens;
}

std::vector<Token> tokenize(std::vector<std::string> lines) {
  std::vector<Token> tokens;
  for (unsigned int row{1}; const auto &line : lines) {
    auto newTokens = tokenize(line, row);
    tokens.insert(tokens.end(), newTokens.begin(), newTokens.end());
    ++row;
  }
  return tokens;
}

std::shared_ptr<SExpr> parseAtom(Token token) {
  if ((token.str.length() >= 1 &&
       all_of(token.str.begin(), token.str.end(), ::isdigit)) ||
      (token.str[0] == '-' && token.str.length() > 1 &&
       all_of(token.str.begin() + 1, token.str.end(), ::isdigit))) {
    return std::make_shared<IntAtom>(stoi(token.str));
  }
  if (token.str.front() == '\"' && token.str.back() == '\"') {
    return std::make_shared<StringAtom>(token.str);
  }
  if (token.str == "#t") {
    return std::make_shared<BoolAtom>(true);
  }
  if (token.str == "#f") {
    return std::make_shared<BoolAtom>(false);
  }
  if (token.str == "'") {
    return std::make_shared<SymAtom>("quote");
  }
  if (token.str == "`") {
    return std::make_shared<SymAtom>("quasiquote");
  }
  if (token.str == ",") {
    return std::make_shared<SymAtom>("unquote");
  }
  if (token.str == ",@") {
    return std::make_shared<SymAtom>("unquote-splicing");
  }
  return std::make_shared<SymAtom>(token.str);
}

std::shared_ptr<SExpr> parse(std::vector<Token>::const_iterator &it,
                             SourceLoc &sourceLoc);

std::shared_ptr<SExpr> parseSexprs(std::vector<Token>::const_iterator &it,
                                   SourceLoc &sourceLoc) {
  auto token = *it;
  if (token.str == ")") {
    it += 1;
    auto nil = std::make_shared<NilAtom>();
    sourceLoc.insert({nil, {token.row, token.col - 1}});
    return nil;
  } else if (token.str == "(") {
    it += 1;
    auto first = parseSexprs(it, sourceLoc);
    auto rest = parseSexprs(it, sourceLoc);
    auto sExprs = std::make_shared<SExprs>(first, rest);
    sourceLoc.insert({sExprs, {token.row, token.col - 1}});
    return sExprs;
  }
  auto first = parse(it, sourceLoc);
  auto rest = parseSexprs(it, sourceLoc);
  auto sExprs = std::make_shared<SExprs>(first, rest);
  sourceLoc.insert({sExprs, {token.row, token.col - 1}});
  return sExprs;
}

std::shared_ptr<SExpr> parse(std::vector<Token>::const_iterator &it,
                             SourceLoc &sourceLoc) {
  auto token = *it;
  it += 1;
  if (token.str == "(") {
    auto sExprs = parseSexprs(it, sourceLoc);
    sourceLoc.insert({sExprs, {token.row, token.col}});
    return sExprs;
  }
  if (token.str == "'" || token.str == "`" || token.str == "," ||
      token.str == ",@") {
    auto rest = std::make_shared<SExprs>(parse(it, sourceLoc),
                                         std::make_shared<NilAtom>());
    sourceLoc.insert({rest, {token.row, token.col}});
    auto sExprs = std::make_shared<SExprs>(parseAtom(token), rest);
    sourceLoc.insert({sExprs, {token.row, token.col}});
    return sExprs;
  }
  auto atom = parseAtom(token);
  sourceLoc.insert({atom, {token.row, token.col}});
  return atom;
}

std::shared_ptr<SExpr> parse(std::vector<std::string> lines,
                             SourceLoc &sourceLoc) {
  auto tokens = tokenize(lines);
  std::vector<Token>::const_iterator it = tokens.begin();
  return parse(it, sourceLoc);
}

void handleUnexpectedToken(const Token &token, const std::string &line) {
  std::stringstream ss;
  ss << "Unexpected \"" << token << "\".";
  throw SyntaxError(ss.str(), line, token.row, token.col);
}

void verifyLex(std::string &line, const unsigned int lineNum,
               uint32_t &openParen, uint32_t &closedParen) {
  auto tokens = tokenize(line, lineNum);
  for (auto it = tokens.begin(); it != tokens.end(); ++it) {
    if ((openParen == closedParen && openParen > 0) ||
        (openParen == closedParen && it->str == ")")) {
      handleUnexpectedToken(*it, line);
    }
    if (it->str == "(") {
      openParen += 1;
    } else if (it->str == ")") {
      closedParen += 1;
    }
  }
  if (openParen == 0 && tokens.size() > 1) {
    handleUnexpectedToken(*(tokens.begin() + 1), line);
  }
}
