#include "../error/SyntaxError.hpp"
#include "../sexpr/Bool.hpp"
#include "../sexpr/Nil.hpp"
#include "../sexpr/Num.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/String.hpp"
#include "Compiler.hpp"
#include "Token.hpp"
#include "grammar.hpp"
#include <regex>
#include <sstream>
#include <vector>

using namespace sexpr;
using namespace compile;
using namespace error;

std::vector<Token> Compiler::tokenize(std::vector<std::string> lines) {
  std::vector<Token> tokens;
  for (unsigned int row{1}; const auto &line : lines) {
    auto newTokens = tokenize(line, row);
    tokens.insert(tokens.end(), newTokens.begin(), newTokens.end());
    ++row;
  }
  return tokens;
}

std::vector<Token> Compiler::tokenize(std::string line,
                                      const unsigned int row) {
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

bool Compiler::isNum(const std::string s) {
  try {
    std::stod(s);
  } catch (...) {
    return false;
  }
  return true;
}

const SExprs *Compiler::parse() {
  auto tokens = tokenize(source);
  std::vector<Token>::const_iterator it = tokens.begin();
  const auto res = parse(it, tokens.end());
  if (it != tokens.end()) {
    handleUnexpectedToken(*it, source[it->row - 1]);
  }
  return vm.alloc<SExprs>(res, vm.alloc<Nil>());
}

const SExpr *Compiler::parse(std::vector<Token>::const_iterator &it,
                             const std::vector<Token>::const_iterator &end) {
  auto token = *it;
  it += 1;
  if (token.str == "(") {
    auto sExprs = parseSexprs(it, end);
    sourceLoc.insert({sExprs, {token.row, token.col}});
    return sExprs;
  }
  if (token.str == "'" || token.str == "`" || token.str == "," ||
      token.str == ",@") {
    auto rest = vm.alloc<SExprs>(parse(it, end), vm.alloc<Nil>());
    sourceLoc.insert({rest, {token.row, token.col}});
    auto sExprs = vm.alloc<SExprs>(parseAtom(token), rest);
    sourceLoc.insert({sExprs, {token.row, token.col}});
    return sExprs;
  }
  auto atom = parseAtom(token);
  sourceLoc.insert({atom, {token.row, token.col}});
  return atom;
}

const SExpr *
Compiler::parseSexprs(std::vector<Token>::const_iterator &it,
                      const std::vector<Token>::const_iterator &end) {
  auto token = *it;
  if (token.str == ")") {
    it += 1;
    auto nil = vm.alloc<Nil>();
    sourceLoc.insert({nil, {token.row, token.col - 1}});
    return nil;
  } else if (token.str == "(") {
    it += 1;
    auto first = parseSexprs(it, end);
    auto rest = parseSexprs(it, end);
    auto sExprs = vm.alloc<SExprs>(first, rest);
    sourceLoc.insert({sExprs, {token.row, token.col - 1}});
    return sExprs;
  }
  return parseList(it, end);
}

const SExpr *
Compiler::parseList(std::vector<Token>::const_iterator &it,
                    const std::vector<Token>::const_iterator &end) {
  auto token = *it;
  auto first = parse(it, end);
  const SExpr *rest = nullptr;
  if (it->str == ".") {
    it += 1;
    rest = parse(it, end);
    if (it == end) {
      handleSyntaxError(dotGrammer, "datum", rest);
    }
    it += 1;
  } else {
    rest = parseSexprs(it, end);
  }
  auto sExprs = vm.alloc<SExprs>(first, rest);
  sourceLoc.insert({sExprs, {token.row, token.col - 1}});
  return sExprs;
}

const SExpr *Compiler::parseAtom(Token token) {
  if (isNum(token.str)) {
    return vm.alloc<Num>(std::stod(token.str));
  }
  if (token.str.front() == '\"' && token.str.back() == '\"') {
    return vm.alloc<String>(token.str);
  }
  if (token.str == "#t") {
    return vm.alloc<Bool>(true);
  }
  if (token.str == "#f") {
    return vm.alloc<Bool>(false);
  }
  if (token.str == "'") {
    return vm.alloc<Sym>("quote");
  }
  if (token.str == "`") {
    return vm.alloc<Sym>("quasiquote");
  }
  if (token.str == ",") {
    return vm.alloc<Sym>("unquote");
  }
  if (token.str == ",@") {
    return vm.alloc<Sym>("unquote-splicing");
  }
  return vm.alloc<Sym>(token.str);
}

void Compiler::handleUnexpectedToken(const Token &token,
                                     const std::string &line) {
  std::stringstream ss;
  ss << "Unexpected \"" << token.str << "\".";
  throw SyntaxError(ss.str(), line, token.row, token.col);
}

void Compiler::verifyLex(std::string &line, const unsigned int lineNum,
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
}
