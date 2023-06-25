#include "Compiler.hpp"
#include "../code/OpCode.hpp"
#include "../error/SyntaxError.hpp"
#include "../runtime/VM.hpp"
#include "../sexpr/String.hpp"
#include "../sexpr/cast.cpp"
#include "grammar.hpp"
#include <regex>
#include <sstream>

using namespace code;
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
    tokens.push_back(Token{match.str(),
                           {
                               row,
                               (unsigned int)match.position(),
                           }});
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

const SExprs &Compiler::parse() {
  auto tokens = tokenize(source);
  std::vector<Token>::const_iterator it = tokens.begin();
  const auto &res = parse(it, tokens.end());
  if (it != tokens.end()) {
    handleUnexpectedToken(*it, source[it->srcLoc.row - 1]);
  }
  return vm.alloc<SExprs>(res, vm.alloc<Nil>());
}

const SExpr &Compiler::parse(TokenIter &it, const TokenIter &end) {
  auto token = *it;
  it += 1;
  if (token.str == "(") {
    const auto &sExprs = parseSexprs(it, end);
    srcMap.insert({&sExprs, {token.srcLoc.row, token.srcLoc.col}});
    return sExprs;
  }
  if (token.str == "'" || token.str == "`" || token.str == "," ||
      token.str == ",@") {
    const auto &rest = vm.alloc<SExprs>(parse(it, end), vm.alloc<Nil>());
    srcMap.insert({&rest, {token.srcLoc.row, token.srcLoc.col}});
    const auto &sExprs = vm.alloc<SExprs>(parseAtom(token), rest);
    srcMap.insert({&sExprs, {token.srcLoc.row, token.srcLoc.col}});
    return sExprs;
  }
  const auto &atom = parseAtom(token);
  srcMap.insert({&atom, {token.srcLoc.row, token.srcLoc.col}});
  return atom;
}

const SExpr &Compiler::parseSexprs(TokenIter &it, const TokenIter &end) {
  auto token = *it;
  if (token.str == ")") {
    it += 1;
    const auto &nil = vm.alloc<Nil>();
    srcMap.insert({&nil, {token.srcLoc.row, token.srcLoc.col - 1}});
    return nil;
  } else if (token.str == "(") {
    it += 1;
    const auto &first = parseSexprs(it, end);
    const auto &rest = parseSexprs(it, end);
    const auto &sExprs = vm.alloc<SExprs>(first, rest);
    srcMap.insert({&sExprs, {token.srcLoc.row, token.srcLoc.col - 1}});
    return sExprs;
  }
  return parseList(it, end);
}

const SExpr &Compiler::parseList(TokenIter &it, const TokenIter &end) {
  auto token = *it;
  const auto &first = parse(it, end);
  if (it->str == ".") {
    it += 1;
    const auto &rest = parse(it, end);
    if (it == end) {
      handleSyntaxError(dotGrammer, "datum", rest);
    }
    it += 1;
    const auto &sExprs = vm.alloc<SExprs>(first, rest);
    srcMap.insert({&sExprs, {token.srcLoc.row, token.srcLoc.col - 1}});
    return sExprs;
  }
  const auto &rest = parseSexprs(it, end);
  const auto &sExprs = vm.alloc<SExprs>(first, rest);
  srcMap.insert({&sExprs, {token.srcLoc.row, token.srcLoc.col - 1}});
  return sExprs;
}

const SExpr &Compiler::parseAtom(Token token) {
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
  throw SyntaxError(ss.str(), line, token.srcLoc.row, token.srcLoc.col);
}

Compiler::Compiler(const std::vector<std::string> source, SrcMap sourceLoc,
                   const SExpr &param, const SExprs &body, Compiler *enclosing,
                   VM &vm)
    : vm(vm), enclosing(enclosing), source(source), srcMap(sourceLoc),
      params(param), body(body), stackOffset(1) {
  if (isa<SExprs>(param)) {
    visitEach(cast<SExprs>(param), [&](const auto &sExpr) {
      const auto &sym = cast<Sym>(sExpr);
      locals.push_back({sym, stackOffset, false});
      stackOffset += 1;
    });
  } else if (isa<Sym>(param)) {
    locals.push_back({cast<Sym>(param), stackOffset, false});
    stackOffset += 1;
  } else if (!isa<Nil>(param)) {
    handleSyntaxError(lambdaGrammar, Nil::typeName, param);
  }
}

int Compiler::resolveLocal(const Sym &sym) {
  auto it =
      std::find_if(locals.rbegin(), locals.rend(),
                   [&](const auto &local) { return local.symbol == sym; });
  if (it == locals.rend()) {
    return -1;
  }
  return std::distance(begin(locals), it.base()) - 1;
}

int Compiler::resolveUpvalue(Compiler &caller, const Sym &sym) {
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
                             [=](const auto upValue) {
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
  return visitEach(params, [](const auto &sExpr) {});
}

unsigned int Compiler::visitEach(const SExpr &sExpr, Visitor visitor) {
  if (!isa<SExprs>(sExpr)) {
    return 0;
  }
  const auto &sExprs = cast<SExprs>(sExpr);
  visitor(sExprs.first);
  return 1 + visitEach(sExprs.rest, visitor);
}

void Compiler::traverse(const SExpr &sExpr, Visitor visitor) {
  if (isa<SExprs>(sExpr)) {
    const auto &sexprs = cast<SExprs>(sExpr);
    traverse(sexprs.first, visitor);
    traverse(sexprs.rest, visitor);
  }
  visitor(sExpr);
}

const SExpr &Compiler::at(const unsigned int n, const SExpr &sExpr) {
  if (n == 0) {
    return sExpr;
  }
  const auto &sexprs = cast<SExprs>(sExpr);
  return at(n - 1, sexprs.rest);
}

void Compiler::compileStmt(const SExpr &sExpr) {
  if (isa<SExprs>(sExpr)) {
    const auto &sExprs = cast<SExprs>(sExpr);
    if (isa<Sym>(sExprs.first)) {
      const auto &sym = cast<Sym>(sExprs.first);
      if (sym.val == "define") {
        compileDef(sExpr);
        return;
      } else if (sym.val == "defmacro") {
        compileDefMacro(sExpr);
        return;
      } else if (sym.val == "begin") {
        code.pushCode(OpCode::MAKE_NIL, srcMap[&sym].row);
        visitEach(sExprs.rest, [&](const auto &sExpr) {
          stackOffset += 1;
          this->compileStmt(sExpr);
        });
        return;
      }
    }
  }
  compileExpr(sExpr);
}

void Compiler::compileExpr(const SExpr &sExpr) {
  if (isa<Atom>(sExpr)) {
    const auto &atom = cast<Atom>(sExpr);
    compileAtom(atom);
    return;
  }
  const auto &sExprs = cast<SExprs>(sExpr);
  if (isa<Sym>(sExprs.first)) {
    const auto &sym = cast<Sym>(sExprs.first);
    if (vm.isMacro(sym)) {
      compileExpr(expandMacro(sExpr));
      return;
    } else if (sym.val == "begin") {
      const auto lineNum = srcMap[&sExpr].row;
      code.pushCode(OpCode::MAKE_NIL, lineNum);

      visitEach(sExprs.rest, [&](const auto &sExpr) {
        code.pushCode(OpCode::POP_TOP, lineNum);
        this->compileExpr(sExpr);
      });
      return;
    } else if (sym.val == "quote") {
      compileQuote(sExpr);
      return;
    } else if (sym.val == "set!") {
      compileSet(sExpr);
      return;
    } else if (sym.val == "if") {
      compileIf(sExpr);
      return;
    } else if (sym.val == "lambda") {
      compileLambda(sExpr);
      return;
    } else if (sym.val == "define" || sym.val == "defmacro") {
      const auto [row, col] = srcMap[&sExpr];
      throw error::SyntaxError(
          "Invalid syntax for define: cannot use define as an expression",
          source[row - 1], row, col);
    }
  }
  compileCall(sExprs);
}

void Compiler::compileLambda(const SExpr &sExpr) {
  try {
    const auto &argNames = cast<SExprs>(at(lambdaArgPos, sExpr)).first;
    const auto &body = cast<SExprs>(at(lambdaBodyPos, sExpr));

    Compiler compiler(source, srcMap, argNames, body, this, vm);
    const auto &function = compiler.compile();

    const auto &lineNum = srcMap[&sExpr].row;
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

void Compiler::compileCall(const SExprs &sExprs) {
  compileExpr(sExprs.first);
  const auto argc = visitEach(
      sExprs.rest, [&](const auto &sExpr) { this->compileExpr(sExpr); });

  const auto lineNum = srcMap[&sExprs.first].row;
  code.pushCode(OpCode::CALL, lineNum);
  code.pushCode(argc, lineNum);
}

void Compiler::compileAtom(const Atom &atom) {
  if (isa<Sym>(atom)) {
    compileSym(cast<Sym>(atom));
    return;
  }
  const auto lineNum = srcMap[&atom].row;
  code.pushCode(OpCode::LOAD_CONST, lineNum);
  code.pushCode(code.pushConst(atom), lineNum);
}

void Compiler::compileSym(const Sym &sym) {
  const auto lineNum = srcMap[&sym].row;

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

void Compiler::compileQuote(const SExpr &sExpr) {
  const auto &expr = cast<SExprs>(at(quoteArgPos, sExpr)).first;
  cast<Nil>(at(quoteNilPos, sExpr));

  const auto lineNum = srcMap[&sExpr].row;
  code.pushCode(OpCode::LOAD_CONST, lineNum);
  code.pushCode(code.pushConst(expr), lineNum);
}

void Compiler::compileDef(const SExpr &sExpr) {
  try {
    const auto &sym = cast<Sym>(cast<SExprs>(at(defSymPos, sExpr)).first);
    const auto &expr = cast<SExprs>(at(defSExprPos, sExpr)).first;
    cast<Nil>(at(defNilPos, sExpr));

    compileExpr(expr);

    const auto lineNum = srcMap[&sExpr].row;
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

void Compiler::compileDefMacro(const SExpr &sExpr) {
  if (enclosing) {
    const auto [row, col] = srcMap[&sExpr];
    throw error::SyntaxError(
        "Invalid syntax for define-macro: must define macros in top level",
        source[row - 1], row, col);
  }
  try {
    const auto &sym = cast<Sym>(cast<SExprs>(at(defMacroSymPos, sExpr)).first);
    const auto &argNames = cast<SExprs>(at(defMacroArgPos, sExpr)).first;
    const auto &body = cast<SExprs>(at(defMacroBodyPos, sExpr));

    Compiler compiler(source, srcMap, argNames, body, this, vm);
    const auto &function = compiler.compile();

    const auto lineNum = srcMap[&sExpr].row;
    code.pushCode(OpCode::MAKE_CLOSURE, lineNum);
    code.pushCode(code.pushConst(function), lineNum);

    code.pushCode(OpCode::DEF_SYM, lineNum);
    code.pushCode(code.pushConst(sym), lineNum);

    vm.defMacro(sym);
  } catch (error::TypeError &te) {
    handleSyntaxError(defMacroGrammar, te.expected, te.actual);
  }
}

void Compiler::compileSet(const SExpr &sExpr) {
  try {
    const auto &sym = cast<Sym>(cast<SExprs>(at(setSymPos, sExpr)).first);
    const auto &expr = cast<SExprs>(at(setSExprPos, sExpr)).first;
    cast<Nil>(at(setNilPos, sExpr));

    compileExpr(expr);

    const auto lineNum = srcMap[&sExpr].row;

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

void Compiler::compileIf(const SExpr &sExpr) {
  try {
    const auto &test = cast<SExprs>(at(ifTestPos, sExpr)).first;
    const auto &conseq = cast<SExprs>(at(ifConseqPos, sExpr)).first;
    const auto &alt = cast<SExprs>(at(ifAltPos, sExpr)).first;
    cast<Nil>(at(ifNilPos, sExpr));

    compileExpr(test);

    const auto testLoc = srcMap[&test].row;
    const auto jifIdx = code.pushCode(OpCode::POP_JUMP_IF_FALSE, testLoc) + 1;
    code.pushCode(UINT8_MAX, testLoc);
    code.pushCode(UINT8_MAX, testLoc);

    compileExpr(conseq);

    const auto conseqLoc = srcMap[&conseq].row;
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
      const auto lineNum = srcMap[&local->symbol].row;

      if (local->isCaptured) {
        code.pushCode(OpCode::CLOSE_UPVALUE, lineNum);
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

const SExpr &Compiler::expandMacro(const SExpr &sExpr) {
  Code fexpr;

  const auto &sExprs = cast<SExprs>(sExpr);

  const auto lineNum = srcMap[&sExprs.first].row;
  const auto colNum = srcMap[&sExprs.first].col;

  fexpr.pushCode(OpCode::LOAD_SYM, lineNum);
  fexpr.pushCode(fexpr.pushConst(sExprs.first), lineNum);

  const auto argc = visitEach(sExprs.rest, [&](const auto &sExpr) {
    fexpr.pushCode(OpCode::LOAD_CONST, lineNum);
    fexpr.pushCode(fexpr.pushConst(sExpr), lineNum);
  });

  fexpr.pushCode(OpCode::CALL, lineNum);
  fexpr.pushCode(argc, lineNum);
  fexpr.pushCode(OpCode::RETURN, lineNum);

  const auto &res = vm.eval(vm.alloc<Fn>(0, 0, fexpr));

  traverse(res, [&](const auto &sExpr) {
    srcMap.insert({&sExpr, {lineNum, colNum}});
  });

  return res;
}

void Compiler::handleSyntaxError(const std::string grammar,
                                 const std::string expected,
                                 const SExpr &actual) {
  std::stringstream ss;
  ss << "Invalid syntax for " << grammar << "." << std::endl
     << "Expected " << expected << ", but got " << actual << ".";
  const auto [row, col] = srcMap[&actual];
  throw SyntaxError(ss.str(), source[row - 1], row, col);
}

Compiler::Compiler(std::vector<std::string> source, VM &vm)
    : vm(vm), enclosing(nullptr), source(source), params(vm.alloc<Nil>()),
      body(parse()), stackOffset(1) {}

const Fn &Compiler::compile() {
  const auto lineNum = srcMap[&body].row;

  if (isVariadic()) {
    code.pushCode(OpCode::MAKE_LIST, lineNum);
  }

  code.pushCode(OpCode::MAKE_NIL, lineNum);
  visitEach(body, [&](const auto &sExpr) {
    stackOffset += 1;
    this->compileStmt(sExpr);
  });
  compileRet();

  return vm.alloc<Fn>(countParams(), upValues.size(), code);
}

void Compiler::verifyLex(std::string &line, const unsigned int lineNum,
                         unsigned int &openParen, unsigned int &closedParen) {
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
