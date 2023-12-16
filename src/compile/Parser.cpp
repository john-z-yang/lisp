#include "Parser.hpp"
#include "../error/SyntaxError.hpp"
#include "../sexpr/Casting.hpp"
#include "../sexpr/Num.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/Sym.hpp"
#include "Grammar.hpp"
#include "ParsedSrc.hpp"
#include <regex>

using namespace compile;
using namespace sexpr;
using namespace error;

bool Parser::isNum(const std::string s) {
  try {
    std::stod(s);
  } catch (...) {
    return false;
  }
  return true;
}

std::vector<Token> Parser::tokenize(std::vector<std::string> lines) {
  std::vector<Token> tokens;
  for (unsigned int row{1}; const auto &line : lines) {
    auto newTokens = tokenize(line, row);
    tokens.insert(tokens.cend(), newTokens.cbegin(), newTokens.cend());
    ++row;
  }
  return tokens;
}

std::vector<Token> Parser::tokenize(std::string line, const unsigned int row) {
  std::vector<Token> tokens;
  std::regex rgx(
      "\\\"(?:[^\"\\\\]*(?:\\\\.)?)*\\\"|;|\\(|\\)|,@|,|`|'|[^\\s(),@,`']+"
  );
  auto begin = std::sregex_iterator(line.cbegin(), line.cend(), rgx);
  auto end = std::sregex_iterator();
  for (std::sregex_iterator i = begin; i != end; ++i) {
    std::smatch match = *i;
    tokens.push_back(Token{
        match.str(),
        {
            row,
            (unsigned int)match.position(),
        }
    });
  }
  return tokens;
}

const sexpr::SExprs *Parser::parse() {
  auto tokens = tokenize(source);
  auto it = tokens.cbegin();
  return cast<SExprs>(parseLists(it, tokens.cend()));
}

const SExpr *Parser::parseLists(TokenIter &it, const TokenIter &end) {
  if (it == end) {
    return vm.heap.alloc<Nil>();
  }
  const auto [row, col] = it->srcLoc;
  const auto cur = parseList(it, end);
  const auto sexprs = vm.heap.alloc<SExprs>(cur, parseLists(it, end));
  srcMap[sexprs] = {row, col};
  return sexprs;
}

const SExpr *Parser::parseList(TokenIter &it, const TokenIter &end) {
  auto token = *it;
  it += 1;
  if (token.str == "(") {
    const auto sExprs = parseElem(it, end);
    srcMap.insert({sExprs, {token.srcLoc.row, token.srcLoc.col}});
    return sExprs;
  }
  if (token.str == "'" || token.str == "`" || token.str == "," ||
      token.str == ",@") {
    const auto rest =
        vm.heap.alloc<SExprs>(parseList(it, end), vm.heap.alloc<Nil>());
    srcMap.insert({rest, {token.srcLoc.row, token.srcLoc.col}});
    const auto sExprs = vm.heap.alloc<SExprs>(parseAtom(token), rest);
    srcMap.insert({sExprs, {token.srcLoc.row, token.srcLoc.col}});
    return sExprs;
  }
  const auto atom = parseAtom(token);
  return atom;
}

const SExpr *Parser::parseElem(TokenIter &it, const TokenIter &end) {
  auto token = *it;
  if (token.str == ")") {
    it += 1;
    return vm.heap.alloc<Nil>();
  } else if (token.str == "(") {
    it += 1;
    const auto first = parseElem(it, end);
    const auto rest = parseElem(it, end);
    const auto sExprs = vm.heap.alloc<SExprs>(first, rest);
    srcMap.insert({sExprs, {token.srcLoc.row, token.srcLoc.col}});
    return sExprs;
  }
  return parseSexprs(it, end);
}

const SExpr *Parser::parseSexprs(TokenIter &it, const TokenIter &end) {
  auto token = *it;
  const auto first = parseList(it, end);
  if (it->str == ".") {
    it += 1;
    const auto rest = parseList(it, end);
    if (it == end) {
      std::stringstream ss;
      ss << "Invalid syntax for " << dotGrammer << "." << std::endl
         << "Expected datum, but got " << rest << ".";
      const auto [row, col] = it->srcLoc;
      throw SyntaxError(ss.str(), source[row - 1], row, col);
    }
    it += 1;
    const auto sExprs = vm.heap.alloc<SExprs>(first, rest);
    srcMap.insert({sExprs, {token.srcLoc.row, token.srcLoc.col}});
    return sExprs;
  }
  const auto rest = parseElem(it, end);
  const auto sExprs = vm.heap.alloc<SExprs>(first, rest);
  srcMap.insert({sExprs, {token.srcLoc.row, token.srcLoc.col}});
  return sExprs;
}

const SExpr *Parser::parseAtom(Token token) {
  if (isNum(token.str)) {
    return vm.heap.alloc<Num>(std::stod(token.str));
  }
  if (token.str.front() == '\"' && token.str.back() == '\"') {
    return vm.heap.alloc<String>(token.str);
  }
  if (token.str == "#<undefined>") {
    return vm.heap.alloc<Undefined>();
  }
  if (token.str == "#t") {
    return vm.heap.alloc<Bool>(true);
  }
  if (token.str == "#f") {
    return vm.heap.alloc<Bool>(false);
  }
  if (token.str == "'") {
    return vm.heap.alloc<Sym>("quote");
  }
  if (token.str == "`") {
    return vm.heap.alloc<Sym>("quasiquote");
  }
  if (token.str == ",") {
    return vm.heap.alloc<Sym>("unquote");
  }
  if (token.str == ",@") {
    return vm.heap.alloc<Sym>("unquote-splicing");
  }
  return vm.heap.alloc<Sym>(token.str);
}

void Parser::verifyLex(
    const std::string &line,
    const unsigned int curSrcLoc,
    unsigned int &openParen,
    unsigned int &closedParen
) {
  auto tokens = tokenize(line, curSrcLoc);
  for (auto it = tokens.cbegin(); it != tokens.cend(); ++it) {
    if (openParen == closedParen && it->str == ")") {
      std::stringstream ss;
      ss << "Unexpected \"" << it->str << "\".";
      throw SyntaxError(ss.str(), line, it->srcLoc.row, it->srcLoc.col);
    }
    if (it->str == "(") {
      openParen += 1;
    } else if (it->str == ")") {
      closedParen += 1;
    }
  }
}

Parser::Parser(runtime::VM &vm, const std::vector<std::string> source)
    : vm(vm),
      gcGuard(vm.heap.pauseGC()),
      source(source),
      parsedSrc({source, srcMap, parse()}) {}

const ParsedSrc Parser::getParsed() const { return parsedSrc; }
