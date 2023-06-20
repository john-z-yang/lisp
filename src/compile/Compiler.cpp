#include "Compiler.hpp"
#include "../common/OpCode.hpp"
#include "../error/SyntaxError.hpp"
#include "../runtime/VM.hpp"
#include "../sexpr/String.hpp"
#include "../sexpr/cast.cpp"
#include "grammar.hpp"
#include <regex>
#include <sstream>

using namespace sexpr;
using namespace compile;
using namespace runtime;
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

Compiler::Compiler(const std::vector<std::string> source, SourceLoc sourceLoc,
                   const SExpr *param, const SExprs *body, Compiler *enclosing,
                   VM &vm)
    : vm(vm), enclosing(enclosing), source(source), sourceLoc(sourceLoc),
      params(param), body(body), stackOffset(1) {
  if (const auto argNames = dynCast<SExprs>(param)) {
    visitEach(argNames, [&](const SExpr *sExpr) {
      auto sym = cast<Sym>(sExpr);
      locals.push_back({sym, stackOffset, false});
      stackOffset += 1;
    });
  } else if (const auto argName = dynCast<Sym>(param)) {
    locals.push_back({argName, stackOffset, false});
    stackOffset += 1;
  } else if (!isa<Nil>(param)) {
    handleSyntaxError(lambdaGrammar, Nil::typeName, param);
  }
}

int Compiler::resolveLocal(const Sym *sym) {
  auto it = std::find_if(locals.rbegin(), locals.rend(),
                         [&sym](Local local) { return *local.symbol == *sym; });
  if (it == locals.rend()) {
    return -1;
  }
  return std::distance(begin(locals), it.base()) - 1;
}

int Compiler::resolveUpvalue(Compiler &caller, const Sym *sym) {
  if (enclosing) {
    if (auto idx = enclosing->resolveLocal(sym); idx != -1) {
      enclosing->locals[idx].isCaptured = true;
      return caller.addUpvalue(enclosing->locals[idx].stackOffset, true);
    }
    if (auto idx = enclosing->resolveUpvalue(*enclosing, sym); idx != -1) {
      return caller.addUpvalue(idx, false);
    }
  }
  return -1;
}

int Compiler::addUpvalue(int idx, bool isLocal) {
  if (auto it = std::find_if(upValues.begin(), upValues.end(),
                             [=](Upvalue upValue) {
                               return upValue.idx == idx &&
                                      upValue.isLocal == isLocal;
                             });
      it != upValues.end()) {
    return std::distance(upValues.begin(), it);
  }
  upValues.push_back({idx, isLocal});
  return upValues.size() - 1;
}

bool Compiler::isVariadic() { return isa<Sym>(params); }

int Compiler::countParams() {
  if (isVariadic()) {
    return -1;
  }
  return visitEach(params, [](const SExpr *sExpr) {});
}

unsigned int Compiler::visitEach(const SExpr *sExprs, Visitor visitor) {
  auto numVisited = 0U;
  auto cur = sExprs;
  while (isa<SExprs>(cur)) {
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

void Compiler::compileStmt(const SExpr *sExpr) {
  if (const auto sExprs = dynCast<SExprs>(sExpr)) {
    if (const auto sym = dynCast<Sym>(sExprs->first)) {
      if (sym->val == "define") {
        compileDef(sExpr);
        return;
      } else if (sym->val == "defmacro") {
        compileDefMacro(sExpr);
        return;
      } else if (sym->val == "begin") {
        code.pushCode(OpCode::MAKE_NIL);
        visitEach(sExprs->rest, [&](const SExpr *sExpr) {
          stackOffset += 1;
          this->compileStmt(sExpr);
        });
        return;
      }
    }
  }
  compileExpr(sExpr);
}

void Compiler::compileExpr(const SExpr *sExpr) {
  if (const auto atom = dynCast<Atom>(sExpr)) {
    compileAtom(atom);
    return;
  }
  const auto sExprs = cast<SExprs>(sExpr);
  if (const auto sym = dynCast<Sym>(sExprs->first)) {
    if (vm.isMacro(sym)) {
      compileExpr(expandMacro(sExpr));
      return;
    } else if (sym->val == "begin") {
      code.pushCode(OpCode::MAKE_NIL);
      visitEach(sExprs->rest, [&](const SExpr *sExpr) {
        code.pushCode(OpCode::POP_TOP);
        this->compileExpr(sExpr);
      });
      return;
    } else if (sym->val == "quote") {
      compileQuote(sExpr);
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
    } else if (sym->val == "define" || sym->val == "defmacro") {
      const auto [row, col] = sourceLoc[sExpr];
      throw error::SyntaxError(
          "Invalid syntax for define: cannot use define as an expression",
          source[row - 1], row, col);
    }
  }
  compileCall(sExprs);
}

void Compiler::compileLambda(const SExpr *sExpr) {
  try {
    const auto argNames = cast<SExprs>(at(lambdaArgPos, sExpr))->first;
    const auto body = cast<SExprs>(at(lambdaBodyPos, sExpr));

    Compiler compiler(source, sourceLoc, argNames, body, this, vm);
    const auto function = compiler.compile();

    const auto lineNum = std::get<0>(sourceLoc[sExpr]);
    code.pushCode(OpCode::MAKE_CLOSURE, lineNum);
    code.pushCode(code.pushConst(function), lineNum);
    for (const auto &upValue : compiler.upValues) {
      code.pushCode(upValue.isLocal ? 1 : 0);
      code.pushCode(upValue.idx);
    }
  } catch (error::TypeError &te) {
    handleSyntaxError(lambdaGrammar, te.expected, te.actual);
  }
}

void Compiler::compileCall(const SExprs *sExprs) {
  compileExpr(sExprs->first);
  const auto argc = visitEach(
      sExprs->rest, [&](const SExpr *sExpr) { this->compileExpr(sExpr); });

  const auto lineNum = std::get<0>(sourceLoc[sExprs->first]);
  code.pushCode(OpCode::CALL, lineNum);
  code.pushCode(argc, lineNum);
}

void Compiler::compileAtom(const Atom *atom) {
  if (const auto symAtom = dynCast<Sym>(atom)) {
    compileSym(cast<Sym>(atom));
    return;
  }
  const auto lineNum = std::get<0>(sourceLoc[atom]);
  code.pushCode(OpCode::LOAD_CONST, lineNum);
  code.pushCode(code.pushConst(atom), lineNum);
}

void Compiler::compileSym(const Sym *sym) {
  const auto lineNum = std::get<0>(sourceLoc[sym]);

  if (const auto idx = resolveLocal(sym); idx != -1) {
    code.pushCode(OpCode::LOAD_STACK, lineNum);
    code.pushCode((uint8_t)locals[idx].stackOffset, lineNum);
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
  cast<Nil>(at(quoteNilPos, sExpr));

  const auto lineNum = std::get<0>(sourceLoc[sExpr]);
  code.pushCode(OpCode::LOAD_CONST, lineNum);
  code.pushCode(code.pushConst(expr), lineNum);
}

void Compiler::compileDef(const SExpr *sExpr) {
  try {
    const auto sym = cast<Sym>(cast<SExprs>(at(defSymPos, sExpr))->first);
    const auto expr = cast<SExprs>(at(defSExprPos, sExpr))->first;
    cast<Nil>(at(defNilPos, sExpr));

    compileExpr(expr);

    const auto lineNum = std::get<0>(sourceLoc[sExpr]);
    if (enclosing == nullptr) {
      code.pushCode(OpCode::DEF_SYM, lineNum);
      code.pushCode(code.pushConst(sym), lineNum);
    } else {
      locals.push_back({sym, stackOffset, false});
    }
  } catch (error::TypeError &te) {
    handleSyntaxError(defGrammar, te.expected, te.actual);
  }
}

void Compiler::compileDefMacro(const SExpr *sExpr) {
  if (enclosing) {
    const auto [row, col] = sourceLoc[sExpr];
    throw error::SyntaxError(
        "Invalid syntax for define-macro: must define macros in top level",
        source[row - 1], row, col);
  }
  try {
    const auto sym = cast<Sym>(cast<SExprs>(at(defMacroSymPos, sExpr))->first);
    const auto argNames = cast<SExprs>(at(defMacroArgPos, sExpr))->first;
    const auto body = cast<SExprs>(at(defMacroBodyPos, sExpr));

    Compiler compiler(source, sourceLoc, argNames, body, this, vm);
    const auto function = compiler.compile();

    const auto lineNum = std::get<0>(sourceLoc[sExpr]);
    code.pushCode(OpCode::MAKE_CLOSURE, lineNum);
    code.pushCode(code.pushConst(function), lineNum);

    code.pushCode(OpCode::DEF_SYM, lineNum);
    code.pushCode(code.pushConst(sym), lineNum);

    vm.defMacro(sym);
  } catch (error::TypeError &te) {
    handleSyntaxError(defMacroGrammar, te.expected, te.actual);
  }
}

void Compiler::compileSet(const SExpr *sExpr) {
  try {
    const auto sym = cast<Sym>(cast<SExprs>(at(setSymPos, sExpr))->first);
    const auto expr = cast<SExprs>(at(setSExprPos, sExpr))->first;
    cast<Nil>(at(setNilPos, sExpr));

    compileExpr(expr);

    const auto lineNum = std::get<0>(sourceLoc[sExpr]);

    if (const auto idx = resolveLocal(sym); idx != -1) {
      code.pushCode(OpCode::SET_STACK, lineNum);
      code.pushCode(locals[idx].stackOffset, lineNum);
      return;
    }
    if (const auto idx = resolveUpvalue(*this, sym); idx != -1) {
      code.pushCode(OpCode::SET_UPVALUE, lineNum);
      code.pushCode((uint8_t)idx, lineNum);
      return;
    }
    code.pushCode(OpCode::SET_SYM, lineNum);
    code.pushCode(code.pushConst(sym), lineNum);
  } catch (error::TypeError &te) {
    handleSyntaxError(setGrammar, te.expected, te.actual);
  }
}

void Compiler::compileIf(const SExpr *sExpr) {
  try {
    const auto test = cast<SExprs>(at(ifTestPos, sExpr))->first;
    const auto conseq = cast<SExprs>(at(ifConseqPos, sExpr))->first;
    const auto alt = cast<SExprs>(at(ifAltPos, sExpr))->first;
    cast<Nil>(at(ifNilPos, sExpr));

    compileExpr(test);

    const auto testLoc = std::get<0>(sourceLoc[test]);
    const auto jifIdx = code.pushCode(OpCode::POP_JUMP_IF_FALSE, testLoc) + 1;
    code.pushCode(UINT8_MAX, testLoc);
    code.pushCode(UINT8_MAX, testLoc);

    compileExpr(conseq);

    const auto conseqLoc = std::get<0>(sourceLoc[conseq]);
    const auto jIdx = code.pushCode(OpCode::JUMP, conseqLoc) + 1;
    code.pushCode(UINT8_MAX, conseqLoc);
    code.pushCode(UINT8_MAX, conseqLoc);
    code.patchJump(jifIdx);

    compileExpr(alt);

    code.patchJump(jIdx);
  } catch (error::TypeError &te) {
    handleSyntaxError(ifGrammar, te.expected, te.actual);
  }
}

void Compiler::compileRet() {
  code.pushCode(OpCode::SET_STACK);
  code.pushCode(0);

  auto local = locals.rbegin();
  for (auto curOffset{stackOffset}; curOffset > 0; --curOffset) {
    if (local != locals.rend() && local->stackOffset == curOffset) {
      if (local->isCaptured) {
        code.pushCode(OpCode::CLOSE_UPVALUE);
      } else {
        code.pushCode(OpCode::POP_TOP);
      }
      ++local;
      continue;
    }
    code.pushCode(OpCode::POP_TOP);
  }
  code.pushCode(OpCode::RETURN);
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

  return vm.eval(vm.alloc<Fn>(0, 0, macro));
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
    : vm(vm), enclosing(nullptr), source(source), params(vm.alloc<Nil>()),
      body(parse()), stackOffset(1) {}

const Fn *Compiler::compile() {
  if (isVariadic()) {
    code.pushCode(OpCode::MAKE_LIST);
  }

  code.pushCode(OpCode::MAKE_NIL);
  visitEach(body, [&](const SExpr *sExpr) {
    stackOffset += 1;
    this->compileStmt(sExpr);
  });
  compileRet();

  return vm.alloc<Fn>(countParams(), upValues.size(), code);
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
