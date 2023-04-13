#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/StringAtom.hpp"
#include "../sexpr/SymAtom.hpp"
#include "SourceLoc.hpp"
#include "SyntaxError.hpp"
#include "Token.hpp"
#include <algorithm>
#include <iterator>
#include <memory>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

std::vector<Token> tokenize(std::string line, const unsigned int lineNum) {
  std::vector<Token> tokens;
  std::regex rgx(
      "\\\"(?:[^\"\\\\]*(?:\\\\.)?)*\\\"|;|\\(|\\)|,@|,|`|'|[^\\s(),@,`']+");
  std::transform(std::sregex_token_iterator(line.begin(), line.end(), rgx, 0),
                 std::sregex_token_iterator(), back_inserter(tokens),
                 [=](auto str) -> Token {
                   return {lineNum, str};
                 });
  return tokens;
}

std::vector<Token> tokenize(std::vector<std::string> lines) {
  std::vector<Token> tokens;
  for (unsigned int lineNum = 1; const auto &line : lines) {
    auto newTokens = tokenize(line, lineNum);
    tokens.insert(tokens.end(), newTokens.begin(), newTokens.end());
    lineNum += 1;
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
    sourceLoc.insert({nil, token.lineNum});
    return nil;
  } else if (token.str == "(") {
    it += 1;
    auto first = parseSexprs(it, sourceLoc);
    auto rest = parseSexprs(it, sourceLoc);
    auto sExprs = std::make_shared<SExprs>(first, rest);
    sourceLoc.insert({sExprs, token.lineNum});
    return sExprs;
  }
  auto first = parse(it, sourceLoc);
  auto rest = parseSexprs(it, sourceLoc);
  auto sExprs = std::make_shared<SExprs>(first, rest);
  sourceLoc.insert({sExprs, token.lineNum});
  return sExprs;
}

std::shared_ptr<SExpr> parse(std::vector<Token>::const_iterator &it,
                             SourceLoc &sourceLoc) {
  auto token = *it;
  it += 1;
  if (token.str == "(") {
    auto sExprs = parseSexprs(it, sourceLoc);
    sourceLoc.insert({sExprs, token.lineNum});
    return sExprs;
  }
  if (token.str == "'" || token.str == "`" || token.str == "," ||
      token.str == ",@") {
    auto rest = std::make_shared<SExprs>(parse(it, sourceLoc),
                                         std::make_shared<NilAtom>());
    sourceLoc.insert({rest, token.lineNum});
    auto sExprs = std::make_shared<SExprs>(parseAtom(token), rest);
    sourceLoc.insert({sExprs, token.lineNum});
    return sExprs;
  }
  auto atom = parseAtom(token);
  sourceLoc.insert({atom, token.lineNum});
  return atom;
}

std::shared_ptr<SExpr> parse(std::vector<std::string> lines,
                             SourceLoc &sourceLoc) {
  auto tokens = tokenize(lines);
  std::vector<Token>::const_iterator it = tokens.begin();
  return parse(it, sourceLoc);
}

size_t implodeTokens(const std::vector<Token> &tokens,
                     const std::vector<Token>::const_iterator &token,
                     std::string &line) {
  auto pos = 0;
  for (auto it = tokens.begin(); it != tokens.end(); ++it) {
    line += it->str;
    if (distance(it, token) > 0) {
      pos += it->str.length();
    }
    if (it + 1 != tokens.end() && it->str != "(" && it->str != ")" &&
        (it + 1)->str != "(" && (it + 1)->str != ")") {
      line += " ";
      if (distance(it, token) > 0) {
        pos += 1;
      }
    }
  }
  return pos;
}

void handleUnexpectedToken(const std::vector<Token> &tokens,
                           const std::vector<Token>::const_iterator &token) {
  std::stringstream ss;
  ss << "Unexpected \"" << *token << "\".";
  std::string line;
  auto pos = implodeTokens(tokens, token, line);
  throw SyntaxError(ss.str(), line, pos);
}

void verifyLex(std::string &line, uint32_t &openParen, uint32_t &closedParen) {
  auto tokens = tokenize(line, -1);
  for (auto it = tokens.begin(); it != tokens.end(); ++it) {
    if ((openParen == closedParen && openParen > 0) ||
        (openParen == closedParen && it->str == ")")) {
      handleUnexpectedToken(tokens, it);
    }
    if (it->str == "(") {
      openParen += 1;
    } else if (it->str == ")") {
      closedParen += 1;
    }
  }
  if (openParen == 0 && tokens.size() > 1) {
    handleUnexpectedToken(tokens, tokens.begin() + 1);
  }
}
