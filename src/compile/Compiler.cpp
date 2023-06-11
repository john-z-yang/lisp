#include "Compiler.hpp"
#include "../common/Code.hpp"
#include "../common/OpCode.hpp"
#include "../common/TypeError.hpp"
#include "../common/cast.cpp"
#include "../common/sexpr/BoolAtom.hpp"
#include "../common/sexpr/IntAtom.hpp"
#include "../common/sexpr/NilAtom.hpp"
#include "../common/sexpr/SExprs.hpp"
#include "../common/sexpr/StringAtom.hpp"
#include "../common/sexpr/SymAtom.hpp"
#include "../runtime/VM.hpp"
#include "SyntaxError.hpp"
#include "grammar.hpp"
#include <algorithm>
#include <cstddef>
#include <iterator>
#include <memory>
#include <ranges>
#include <regex>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

Compiler::Compiler(const std::vector<std::string> source, SourceLoc sourceLoc,
                   const SExpr *arg, const SExpr *body, Compiler *enclosing,
                   VM &vm)
    : source(source), sourceLoc(sourceLoc), vm(vm), enclosing(enclosing),
      arg(arg), body(body), stackOffset(1) {
  if (const auto argNames = dynamic_cast<const SExprs *>(arg)) {
    visitEach(argNames, [&](const SExpr *sExpr) {
      auto sym = cast<SymAtom>(sExpr);
      locals.push_back({sym, stackOffset, false});
      stackOffset += 1;
    });
  } else if (const auto argName = dynamic_cast<const SymAtom *>(arg)) {
    locals.push_back({argName, stackOffset, false});
    stackOffset += 1;
  } else if (!isa<NilAtom>(*arg)) {
    handleSyntaxError(lambdaGrammar, NilAtom::typeName, arg);
  }
}

std::vector<Compiler::Token>
Compiler::tokenize(std::vector<std::string> lines) {
  std::vector<Token> tokens;
  for (unsigned int row{1}; const auto &line : lines) {
    auto newTokens = tokenize(line, row);
    tokens.insert(tokens.end(), newTokens.begin(), newTokens.end());
    ++row;
  }
  return tokens;
}

std::vector<Compiler::Token> Compiler::tokenize(std::string line,
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

const SExpr *Compiler::parse(std::vector<std::string> lines,
                             SourceLoc &sourceLoc) {
  auto tokens = tokenize(lines);
  std::vector<Token>::const_iterator it = tokens.begin();
  const auto res = parse(it, sourceLoc);
  if (it != tokens.end()) {
    handleUnexpectedToken(*it, lines[it->row - 1]);
  }
  return res;
}

const SExpr *Compiler::parse(std::vector<Token>::const_iterator &it,
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
    auto rest = vm.alloc<SExprs>(parse(it, sourceLoc), vm.alloc<NilAtom>());
    sourceLoc.insert({rest, {token.row, token.col}});
    auto sExprs = vm.alloc<SExprs>(parseAtom(token), rest);
    sourceLoc.insert({sExprs, {token.row, token.col}});
    return sExprs;
  }
  auto atom = parseAtom(token);
  sourceLoc.insert({atom, {token.row, token.col}});
  return atom;
}

const SExpr *Compiler::parseAtom(Token token) {
  if ((token.str.length() >= 1 &&
       all_of(token.str.begin(), token.str.end(), ::isdigit)) ||
      (token.str[0] == '-' && token.str.length() > 1 &&
       all_of(token.str.begin() + 1, token.str.end(), ::isdigit))) {
    return vm.alloc<IntAtom>(stoi(token.str));
  }
  if (token.str.front() == '\"' && token.str.back() == '\"') {
    return vm.alloc<StringAtom>(token.str);
  }
  if (token.str == "#t") {
    return vm.alloc<BoolAtom>(true);
  }
  if (token.str == "#f") {
    return vm.alloc<BoolAtom>(false);
  }
  if (token.str == "'") {
    return vm.alloc<SymAtom>("quote");
  }
  if (token.str == "`") {
    return vm.alloc<SymAtom>("quasiquote");
  }
  if (token.str == ",") {
    return vm.alloc<SymAtom>("unquote");
  }
  if (token.str == ",@") {
    return vm.alloc<SymAtom>("unquote-splicing");
  }
  return vm.alloc<SymAtom>(token.str);
}

const SExpr *Compiler::parseSexprs(std::vector<Token>::const_iterator &it,
                                   SourceLoc &sourceLoc) {
  auto token = *it;
  if (token.str == ")") {
    it += 1;
    auto nil = vm.alloc<NilAtom>();
    sourceLoc.insert({nil, {token.row, token.col - 1}});
    return nil;
  } else if (token.str == "(") {
    it += 1;
    auto first = parseSexprs(it, sourceLoc);
    auto rest = parseSexprs(it, sourceLoc);
    auto sExprs = vm.alloc<SExprs>(first, rest);
    sourceLoc.insert({sExprs, {token.row, token.col - 1}});
    return sExprs;
  }
  auto first = parse(it, sourceLoc);
  auto rest = parseSexprs(it, sourceLoc);
  auto sExprs = vm.alloc<SExprs>(first, rest);
  sourceLoc.insert({sExprs, {token.row, token.col - 1}});
  return sExprs;
}

void Compiler::compile(const SExpr *sExpr) {
  if (isa<NilAtom>(*sExpr) || isa<IntAtom>(*sExpr) || isa<BoolAtom>(*sExpr) ||
      isa<StringAtom>(*sExpr)) {
    const auto lineNum = std::get<0>(sourceLoc[sExpr]);
    code.pushCode(OpCode::LOAD_CONST, lineNum);
    code.pushCode(code.pushConst(sExpr), lineNum);
    return;
  } else if (const auto sym = dynamic_cast<const SymAtom *>(sExpr)) {
    compileSym(sym);
    return;
  }
  const auto sExprs = cast<SExprs>(sExpr);
  if (const auto sym = dynamic_cast<const SymAtom *>(sExprs->first)) {
    if (sym->val == "quote") {
      compileQuote(sExpr);
      return;
    }
    if (sym->val == "define") {
      compileDef(sExpr);
      return;
    } else if (sym->val == "defmacro") {
      compileDefMacro(sExpr);
      return;
    } else if (sym->val == "set!") {
      compileSet(sExpr);
      return;
    } else if (sym->val == "if") {
      compileIf(sExpr);
      return;
    } else if (sym->val == "lambda") {
      compileLambda(sExpr);
      return;
    } else if (vm.isMacro(sym)) {
      compile(expandMacro(sExpr));
      return;
    }
  }
  compileCall(sExprs);
}

void Compiler::compileSym(const SymAtom *sym) {
  const auto lineNum = std::get<0>(sourceLoc[sym]);

  if (const auto idx = resolveLocal(sym); idx != -1) {
    code.pushCode(OpCode::LOAD_STACK, lineNum);
    code.pushCode((uint8_t)idx, lineNum);
    return;
  }
  if (const auto idx = resolveUpvalue(*this, sym); idx != -1) {
    code.pushCode(OpCode::LOAD_UPVALUE, lineNum);
    code.pushCode((uint8_t)idx, lineNum);
    return;
  }
  code.pushCode(OpCode::LOAD_SYM, lineNum);
  code.pushCode(code.pushConst(sym), lineNum);
}

void Compiler::compileQuote(const SExpr *sExpr) {
  const auto expr = cast<SExprs>(at(quoteArgPos, sExpr))->first;
  cast<NilAtom>(at(quoteNilPos, sExpr));

  const auto lineNum = std::get<0>(sourceLoc[sExpr]);
  code.pushCode(OpCode::LOAD_CONST, lineNum);
  code.pushCode(code.pushConst(expr), lineNum);
}

void Compiler::compileDef(const SExpr *sExpr) {
  if ((locals.empty() && stackOffset > 0) ||
      (!locals.empty() && stackOffset > locals.back().stackOffset + 1)) {
    const auto [row, col] = sourceLoc[sExpr];
    throw SyntaxError(
        "Invalid syntax for define: cannot use define as an argument",
        source[row - 1], row, col);
  }
  try {
    const auto sym = cast<SymAtom>(cast<SExprs>(at(defSymPos, sExpr))->first);
    const auto expr = cast<SExprs>(at(defSExprPos, sExpr))->first;
    cast<NilAtom>(at(defNilPos, sExpr));

    compile(expr);

    const auto lineNum = std::get<0>(sourceLoc[sExpr]);
    if (enclosing == nullptr) {
      code.pushCode(OpCode::DEF_SYM, lineNum);
      code.pushCode(code.pushConst(sym), lineNum);
    } else {
      locals.push_back({sym, stackOffset, false});
    }
  } catch (TypeError &te) {
    handleSyntaxError(defGrammar, te.expected, te.actual);
  }
}

void Compiler::compileDefMacro(const SExpr *sExpr) {
  if (enclosing) {
    const auto [row, col] = sourceLoc[sExpr];
    throw SyntaxError(
        "Invalid syntax for define-macro: must define macros in top level",
        source[row - 1], row, col);
  }
  try {
    const auto sym =
        cast<SymAtom>(cast<SExprs>(at(defMacroSymPos, sExpr))->first);
    const auto argNames = cast<SExprs>(at(defMacroArgPos, sExpr))->first;
    const auto body = cast<SExprs>(at(defMacroBodyPos, sExpr))->first;
    cast<NilAtom>(at(defMacroNilPos, sExpr));

    Compiler compiler(source, sourceLoc, argNames, body, this, vm);
    const auto function = compiler.compile();

    const auto lineNum = std::get<0>(sourceLoc[sExpr]);
    code.pushCode(OpCode::MAKE_CLOSURE, lineNum);
    code.pushCode(code.pushConst(function), lineNum);

    code.pushCode(OpCode::DEF_SYM, lineNum);
    code.pushCode(code.pushConst(sym), lineNum);

    vm.defMacro(sym);
  } catch (TypeError &te) {
    handleSyntaxError(defMacroGrammar, te.expected, te.actual);
  }
}

void Compiler::compileSet(const SExpr *sExpr) {
  try {
    const auto sym = cast<SymAtom>(cast<SExprs>(at(setSymPos, sExpr))->first);
    const auto expr = cast<SExprs>(at(setSExprPos, sExpr))->first;
    cast<NilAtom>(at(setNilPos, sExpr));

    compile(expr);

    const auto lineNum = std::get<0>(sourceLoc[sExpr]);

    if (const auto idx = resolveLocal(sym); idx != -1) {
      code.pushCode(OpCode::SET_STACK, lineNum);
      code.pushCode(idx, lineNum);
      return;
    }
    if (const auto idx = resolveUpvalue(*this, sym); idx != -1) {
      code.pushCode(OpCode::SET_UPVALUE, lineNum);
      code.pushCode((uint8_t)idx, lineNum);
      return;
    }
    code.pushCode(OpCode::SET_SYM, lineNum);
    code.pushCode(code.pushConst(sym), lineNum);
  } catch (TypeError &te) {
    handleSyntaxError(setGrammar, te.expected, te.actual);
  }
}

void Compiler::compileIf(const SExpr *sExpr) {
  try {
    const auto test = cast<SExprs>(at(ifTestPos, sExpr))->first;
    const auto conseq = cast<SExprs>(at(ifConseqPos, sExpr))->first;
    const auto alt = cast<SExprs>(at(ifAltPos, sExpr))->first;
    cast<NilAtom>(at(ifNilPos, sExpr));

    compile(test);

    const auto testLoc = std::get<0>(sourceLoc[test]);
    const auto jifIdx = code.pushCode(OpCode::POP_JUMP_IF_FALSE, testLoc) + 1;
    code.pushCode(UINT8_MAX, testLoc);
    code.pushCode(UINT8_MAX, testLoc);

    compile(conseq);

    const auto conseqLoc = std::get<0>(sourceLoc[conseq]);
    const auto jIdx = code.pushCode(OpCode::JUMP, conseqLoc) + 1;
    code.pushCode(UINT8_MAX, conseqLoc);
    code.pushCode(UINT8_MAX, conseqLoc);
    code.patchJump(jifIdx);

    compile(alt);

    code.patchJump(jIdx);
  } catch (TypeError &te) {
    handleSyntaxError(ifGrammar, te.expected, te.actual);
  }
}

void Compiler::compileLambda(const SExpr *sExpr) {
  try {
    const auto argNames = cast<SExprs>(at(lambdaArgPos, sExpr))->first;
    const auto body = cast<SExprs>(at(lambdaBodyPos, sExpr))->first;
    cast<NilAtom>(at(lambdaNilPos, sExpr));

    Compiler compiler(source, sourceLoc, argNames, body, this, vm);
    const auto function = compiler.compile();

    const auto lineNum = std::get<0>(sourceLoc[sExpr]);
    code.pushCode(OpCode::MAKE_CLOSURE, lineNum);
    code.pushCode(code.pushConst(function), lineNum);
    for (const auto &upValue : compiler.upValues) {
      code.pushCode(upValue.isLocal ? 1 : 0);
      code.pushCode(upValue.idx);
    }
  } catch (TypeError &te) {
    handleSyntaxError(lambdaGrammar, te.expected, te.actual);
  }
}

void Compiler::compileCall(const SExprs *sExprs) {
  compile(sExprs->first);
  stackOffset += 1;
  const auto argc = visitEach(sExprs->rest, [&](const SExpr *sExpr) {
    this->compile(sExpr);
    stackOffset += 1;
  });

  const auto lineNum = std::get<0>(sourceLoc[sExprs->first]);
  code.pushCode(OpCode::CALL, lineNum);
  code.pushCode(argc, lineNum);
}

const SExpr *Compiler::expandMacro(const SExpr *sExpr) {
  Code macro;

  const auto sExprs = cast<SExprs>(sExpr);

  macro.pushCode(OpCode::LOAD_SYM);
  macro.pushCode(macro.pushConst(sExprs->first));

  const auto argc = visitEach(sExprs->rest, [&](const SExpr *sExpr) {
    macro.pushCode(OpCode::LOAD_CONST);
    macro.pushCode(macro.pushConst(sExpr));
  });

  macro.pushCode(OpCode::CALL);
  macro.pushCode(argc);
  macro.pushCode(OpCode::RETURN);

  return vm.exec(vm.alloc<FnAtom>(0, 0, macro));
}

unsigned int Compiler::visitEach(const SExpr *sExprs, Visitor visitor) {
  auto numVisited = 0U;
  auto cur = sExprs;
  while (isa<SExprs>(*cur)) {
    auto sExprs = cast<SExprs>(cur);
    visitor(sExprs->first);
    cur = sExprs->rest;
    numVisited += 1;
  }
  return numVisited;
}

const SExpr *Compiler::at(const unsigned int n, const SExpr *sExpr) {
  const SExpr *it = cast<SExprs>(sExpr);
  for (unsigned int i{0}; i < n; ++i) {
    it = cast<SExprs>(it)->rest;
  }
  return it;
}

int Compiler::resolveLocal(const SymAtom *sym) {
  auto it = std::find_if(locals.rbegin(), locals.rend(),
                         [&sym](Local local) { return *local.symbol == *sym; });
  if (it == locals.rend()) {
    return -1;
  }
  return it->stackOffset;
}

int Compiler::resolveUpvalue(Compiler &caller, const SymAtom *sym) {
  if (enclosing) {
    if (auto idx = enclosing->resolveLocal(sym); idx != -1) {
      enclosing->locals[idx - 1].isCaptured = true;
      return caller.addUpvalue(idx, true);
    }
    if (auto idx = enclosing->resolveUpvalue(*enclosing, sym); idx != -1) {
      return caller.addUpvalue(idx, false);
    }
  }
  return -1;
}

int Compiler::addUpvalue(int idx, bool isLocal) {
  if (auto it = std::find_if(upValues.begin(), upValues.end(),
                             [=](UpValue upValue) {
                               return upValue.idx == idx &&
                                      upValue.isLocal == isLocal;
                             });
      it != upValues.end()) {
    return std::distance(upValues.begin(), it);
  }
  upValues.push_back({idx, isLocal});
  return upValues.size() - 1;
}

void Compiler::handleUnexpectedToken(const Token &token,
                                     const std::string &line) {
  std::stringstream ss;
  ss << "Unexpected \"" << token.str << "\".";
  throw SyntaxError(ss.str(), line, token.row, token.col);
}

void Compiler::handleSyntaxError(const std::string grammar,
                                 const std::string expected,
                                 const SExpr *const actual) {
  std::stringstream ss;
  ss << "Invalid syntax for " << grammar << "." << std::endl
     << "Expected " << expected << ", but got " << *actual << ".";
  const auto [row, col] = sourceLoc[actual];
  throw SyntaxError(ss.str(), source[row - 1], row, col);
}

Compiler::Compiler(std::vector<std::string> source, VM &vm)
    : source(source), vm(vm), enclosing(nullptr), arg(vm.alloc<NilAtom>()),
      body(parse(source, sourceLoc)), stackOffset(0) {}

const FnAtom *Compiler::compile() {
  if (isa<SymAtom>(*arg)) {
    code.pushCode(OpCode::MAKE_LIST);
  }

  compile(body);
  code.pushCode(OpCode::SET_STACK);
  code.pushCode(0);
  code.pushCode(OpCode::POP_TOP);

  for (const auto &local : locals | std::views::reverse) {
    if (local.isCaptured) {
      code.pushCode(OpCode::CLOSE_UPVALUE);
    } else {
      code.pushCode(OpCode::POP_TOP);
    }
  }
  code.pushCode(OpCode::RETURN);

  if (isa<SymAtom>(*arg)) {
    return vm.alloc<FnAtom>(-1, upValues.size(), code);
  }

  return vm.alloc<FnAtom>(locals.size(), upValues.size(), code);
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
