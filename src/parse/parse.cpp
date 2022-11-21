#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/StringAtom.hpp"
#include "../sexpr/SymAtom.hpp"
#include "ParseException.hpp"
#include <algorithm>
#include <iterator>
#include <memory>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

std::vector<std::string> tokenize(std::string str) {
  std::vector<std::string> tokens;
  std::regex rgx(
      "\\\"(?:[^\"\\\\]*(?:\\\\.)?)*\\\"|;|\\(|\\)|,@|,|`|'|[^\\s(),@,`']+");
  copy(std::sregex_token_iterator(str.begin(), str.end(), rgx, 0),
       std::sregex_token_iterator(), back_inserter(tokens));
  return tokens;
}

std::shared_ptr<SExpr> parseAtom(std::string token) {
  if ((token.length() >= 1 && all_of(token.begin(), token.end(), ::isdigit)) ||
      (token[0] == '-' && token.length() > 1 &&
       all_of(token.begin() + 1, token.end(), ::isdigit))) {
    return std::make_shared<IntAtom>(stoi(token));
  }
  if (token.front() == '\"' && token.back() == '\"') {
    return std::make_shared<StringAtom>(token);
  }
  if (token == "#t") {
    return std::make_shared<BoolAtom>(true);
  }
  if (token == "#f") {
    return std::make_shared<BoolAtom>(false);
  }
  if (token == "'") {
    return std::make_shared<SymAtom>("quote");
  }
  if (token == "`") {
    return std::make_shared<SymAtom>("quasiquote");
  }
  if (token == ",") {
    return std::make_shared<SymAtom>("unquote");
  }
  if (token == ",@") {
    return std::make_shared<SymAtom>("unquote-splicing");
  }
  return std::make_shared<SymAtom>(token);
}

std::shared_ptr<SExpr> parse(std::vector<std::string>::const_iterator &it);

std::shared_ptr<SExpr>
parseSexprs(std::vector<std::string>::const_iterator &it) {
  std::string token = *it;
  if (token == ")") {
    it += 1;
    return std::make_shared<NilAtom>();
  } else if (token == "(") {
    it += 1;
    std::shared_ptr<SExpr> first = parseSexprs(it);
    std::shared_ptr<SExpr> rest = parseSexprs(it);
    return std::make_shared<SExprs>(first, rest);
  }
  std::shared_ptr<SExpr> first = parse(it);
  std::shared_ptr<SExpr> rest = parseSexprs(it);
  return std::make_shared<SExprs>(first, rest);
}

std::shared_ptr<SExpr> parse(std::vector<std::string>::const_iterator &it) {
  std::string token = *it;
  it += 1;
  if (token == "(") {
    return parseSexprs(it);
  }
  if (token == "'" || token == "`" || token == "," || token == ",@") {
    std::shared_ptr<SExpr> rest =
        std::make_shared<SExprs>(parse(it), std::make_shared<NilAtom>());
    return std::make_shared<SExprs>(parseAtom(token), rest);
  }
  return parseAtom(token);
}

std::shared_ptr<SExpr> parse(std::string str) {
  std::vector<std::string> tokens = tokenize(str);
  std::vector<std::string>::const_iterator it = tokens.begin();
  return parse(it);
}

size_t implodeTokens(const std::vector<std::string> &tokens,
                     const std::vector<std::string>::const_iterator &token,
                     std::string &line) {
  size_t pos = 0;
  for (auto it = tokens.begin(); it != tokens.end(); ++it) {
    line += *it;
    if (distance(it, token) > 0) {
      pos += it->length();
    }
    if (it + 1 != tokens.end() && *it != "(" && *it != ")" &&
        *(it + 1) != "(" && *(it + 1) != ")") {
      line += " ";
      if (distance(it, token) > 0) {
        pos += 1;
      }
    }
  }
  return pos;
}

void handleUnexpectedToken(
    const std::vector<std::string> &tokens,
    const std::vector<std::string>::const_iterator &token) {
  std::stringstream ss;
  ss << "Unexpected \"" << *token << "\".";
  std::string line;
  size_t pos = implodeTokens(tokens, token, line);
  throw ParseException(ss.str(), line, pos);
}

void handleMissingToken(const std::vector<std::string> &tokens,
                        const std::vector<std::string>::const_iterator &token,
                        std::string missing) {
  std::stringstream ss;
  ss << "Expected " << missing << ".";
  std::string line;
  size_t pos = implodeTokens(tokens, token, line);
  throw ParseException(ss.str(), line, pos + token->length());
}

void verifyLex(std::string &line, uint32_t &openParen, uint32_t &closedParen) {
  std::vector<std::string> tokens = tokenize(line);
  for (auto it = tokens.begin(); it != tokens.end(); ++it) {
    if (it->front() == '"' && it->back() != '"') {
      handleMissingToken(tokens, it, "\"");
    }
    if ((openParen == closedParen && openParen > 0) ||
        (openParen == closedParen && *it == ")")) {
      handleUnexpectedToken(tokens, it);
    }
    if (*it == "(") {
      openParen += 1;
    } else if (*it == ")") {
      closedParen += 1;
    }
  }
  if (openParen == 0 && tokens.size() > 1) {
    handleUnexpectedToken(tokens, tokens.begin() + 1);
  }
}

std::istream &getInput(std::istream &in, std::string &str, size_t &linesRead,
                       std::string prompt, std::string wrap) {
  uint32_t openParen = 0;
  uint32_t closedParen = 0;
  std::string line;
  std::cout << prompt;
  while (getline(in, line)) {
    linesRead += 1;
    line = std::regex_replace(
        line, std::regex("(\\\\\"|\"(?:\\\\\"|[^\"])*\")|(;.*$)"), "$1");
    if (str.empty() && line.empty()) {
      std::cout << prompt;
      continue;
    }
    verifyLex(line, openParen, closedParen);
    str += line + " ";
    if (openParen == closedParen) {
      return in;
    }
    std::cout << wrap;
  }
  return in;
}
