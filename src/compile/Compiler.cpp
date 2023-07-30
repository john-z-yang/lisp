#include "Compiler.hpp"
#include "../code/OpCode.hpp"
#include "../error/SyntaxError.hpp"
#include "../runtime/VM.hpp"
#include "../sexpr/Cast.cpp"
#include "../sexpr/String.hpp"
#include "Grammar.hpp"
#include "SrcLoc.hpp"
#include <optional>
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
    tokens.insert(tokens.cend(), newTokens.cbegin(), newTokens.cend());
    ++row;
  }
  return tokens;
}

std::vector<Token> Compiler::tokenize(std::string line,
                                      const unsigned int row) {
  std::vector<Token> tokens;
  std::regex rgx(
      "\\\"(?:[^\"\\\\]*(?:\\\\.)?)*\\\"|;|\\(|\\)|,@|,|`|'|[^\\s(),@,`']+");
  auto begin = std::sregex_iterator(line.cbegin(), line.cend(), rgx);
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
  auto it = tokens.cbegin();
  return cast<SExprs>(parseLists(it, tokens.cend()));
}

const SExpr &Compiler::parseLists(TokenIter &it, const TokenIter &end) {
  if (it == end) {
    return vm.freeStore.alloc<Nil>();
  }
  const auto [row, col] = it->srcLoc;
  const auto &cur = parseList(it, end);
  const auto &sexprs = vm.freeStore.alloc<SExprs>(cur, parseLists(it, end));
  srcMap[&sexprs] = {row, col};
  return sexprs;
}

const SExpr &Compiler::parseList(TokenIter &it, const TokenIter &end) {
  auto token = *it;
  it += 1;
  if (token.str == "(") {
    const auto &sExprs = parseElem(it, end);
    srcMap.insert({&sExprs, {token.srcLoc.row, token.srcLoc.col}});
    return sExprs;
  }
  if (token.str == "'" || token.str == "`" || token.str == "," ||
      token.str == ",@") {
    const auto &rest = vm.freeStore.alloc<SExprs>(parseList(it, end),
                                                  vm.freeStore.alloc<Nil>());
    srcMap.insert({&rest, {token.srcLoc.row, token.srcLoc.col}});
    const auto &sExprs = vm.freeStore.alloc<SExprs>(parseAtom(token), rest);
    srcMap.insert({&sExprs, {token.srcLoc.row, token.srcLoc.col}});
    return sExprs;
  }
  const auto &atom = parseAtom(token);
  return atom;
}

const SExpr &Compiler::parseElem(TokenIter &it, const TokenIter &end) {
  auto token = *it;
  if (token.str == ")") {
    it += 1;
    return vm.freeStore.alloc<Nil>();
  } else if (token.str == "(") {
    it += 1;
    const auto &first = parseElem(it, end);
    const auto &rest = parseElem(it, end);
    const auto &sExprs = vm.freeStore.alloc<SExprs>(first, rest);
    srcMap.insert({&sExprs, {token.srcLoc.row, token.srcLoc.col}});
    return sExprs;
  }
  return parseSexprs(it, end);
}

const SExpr &Compiler::parseSexprs(TokenIter &it, const TokenIter &end) {
  auto token = *it;
  const auto &first = parseList(it, end);
  if (it->str == ".") {
    it += 1;
    const auto &rest = parseList(it, end);
    if (it == end) {
      handleSyntaxError(dotGrammer, "datum", rest);
    }
    it += 1;
    const auto &sExprs = vm.freeStore.alloc<SExprs>(first, rest);
    srcMap.insert({&sExprs, {token.srcLoc.row, token.srcLoc.col}});
    return sExprs;
  }
  const auto &rest = parseElem(it, end);
  const auto &sExprs = vm.freeStore.alloc<SExprs>(first, rest);
  srcMap.insert({&sExprs, {token.srcLoc.row, token.srcLoc.col}});
  return sExprs;
}

const SExpr &Compiler::parseAtom(Token token) {
  if (isNum(token.str)) {
    return vm.freeStore.alloc<Num>(std::stod(token.str));
  }
  if (token.str.front() == '\"' && token.str.back() == '\"') {
    return vm.freeStore.alloc<String>(token.str);
  }
  if (token.str == "#<undefined>") {
    return vm.freeStore.alloc<Undefined>();
  }
  if (token.str == "#t") {
    return vm.freeStore.alloc<Bool>(true);
  }
  if (token.str == "#f") {
    return vm.freeStore.alloc<Bool>(false);
  }
  if (token.str == "'") {
    return vm.freeStore.alloc<Sym>("quote");
  }
  if (token.str == "`") {
    return vm.freeStore.alloc<Sym>("quasiquote");
  }
  if (token.str == ",") {
    return vm.freeStore.alloc<Sym>("unquote");
  }
  if (token.str == ",@") {
    return vm.freeStore.alloc<Sym>("unquote-splicing");
  }
  return vm.freeStore.alloc<Sym>(token.str);
}

void Compiler::handleUnexpectedToken(const Token &token,
                                     const std::string &line) {
  std::stringstream ss;
  ss << "Unexpected \"" << token.str << "\".";
  throw SyntaxError(ss.str(), line, token.srcLoc.row, token.srcLoc.col);
}

Compiler::Compiler(const std::vector<std::string> source, SrcMap sourceLoc,
                   const SExpr &param, const SExprs &body, Compiler &enclosing,
                   VM &vm)
    : vm(vm), enclosing(enclosing), source(source), srcMap(sourceLoc),
      curSrcLoc({srcMap[&param].row, srcMap[&param].col}), argNames(param),
      arity(countArity()), variadic(isVariadic()), body(body), stackOffset(1) {
  if (isa<SExprs>(param)) {
    visitEach(cast<SExprs>(param), [&](const auto &sExpr) {
      const auto &sym = cast<Sym>(sExpr);
      locals.push_back({sym, stackOffset, false});
      stackOffset += 1;
    });
  }

  const auto &lastParam = last(argNames);

  if (isa<Sym>(lastParam)) {
    locals.push_back({cast<Sym>(lastParam), stackOffset, false});
    stackOffset += 1;
  } else if (!isa<Nil>(lastParam)) {
    handleSyntaxError(lambdaGrammar, Nil::getTypeName(), lastParam);
  }
}

void Compiler::updateCurSrcLoc(const sexpr::SExpr &sExpr) {
  if (isa<Atom>(sExpr)) {
    return;
  }
  curSrcLoc = srcMap[&sExpr];
}

std::optional<const std::size_t> Compiler::resolveLocal(const Sym &sym) {
  auto it =
      std::find_if(locals.rbegin(), locals.rend(),
                   [&](const auto &local) { return local.symbol == sym; });
  if (it == locals.rend()) {
    return std::nullopt;
  }
  return std::distance(begin(locals), it.base()) - 1;
}

std::optional<const std::size_t> Compiler::resolveUpvalue(Compiler &caller,
                                                          const Sym &sym) {
  if (!enclosing.has_value()) {
    return std::nullopt;
  }
  if (auto idx = enclosing->get().resolveLocal(sym); idx.has_value()) {
    enclosing->get().locals[idx.value()].isCaptured = true;
    return caller.addUpvalue(enclosing->get().locals[*idx].stackOffset, true);
  }
  if (auto idx = enclosing->get().resolveUpvalue(*enclosing, sym);
      idx.has_value()) {
    return caller.addUpvalue(*idx, false);
  }
  return std::nullopt;
}

std::size_t Compiler::addUpvalue(int idx, bool isLocal) {
  if (auto it = std::find_if(upValues.cbegin(), upValues.cend(),
                             [=](const auto upValue) {
                               return upValue.idx == idx &&
                                      upValue.isLocal == isLocal;
                             });
      it != upValues.end()) {
    return std::distance(upValues.cbegin(), it);
  }
  upValues.push_back({idx, isLocal});
  return upValues.size() - 1;
}

bool Compiler::isVariadic() { return isa<Sym>(last(argNames)); }

uint8_t Compiler::countArity() {
  if (isa<Nil>(argNames) || isa<Sym>(argNames)) {
    return 0;
  }
  return visitEach(cast<SExprs>(argNames),
                   [&]([[maybe_unused]] const auto &sExpr) {});
}

code::InstrPtr Compiler::emitConst(const sexpr::SExpr &sExpr) {
  return code.pushConst(sExpr);
}

void Compiler::patchJump(const code::InstrPtr idx) { code.patchJump(idx); }

const SExpr &Compiler::at(const unsigned int n, const SExpr &sExpr) {
  updateCurSrcLoc(sExpr);
  if (n == 0) {
    return sExpr;
  }
  return at(n - 1, cast<SExprs>(sExpr).rest);
}

const SExpr &Compiler::last(const SExpr &sExpr) {
  updateCurSrcLoc(sExpr);
  if (isa<Atom>(sExpr)) {
    return sExpr;
  }
  return last(cast<SExprs>(sExpr).rest);
}

unsigned int Compiler::visitEach(const SExpr &sExpr, Visitor visitor) {
  if (isa<Atom>(sExpr)) {
    return 0;
  }
  updateCurSrcLoc(sExpr);
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

void Compiler::compileStmt(const SExpr &sExpr) {
  if (isa<SExprs>(sExpr)) {
    const auto &sExprs = cast<SExprs>(sExpr);
    if (isa<Sym>(sExprs.first)) {
      const auto &sym = cast<Sym>(sExprs.first);
      if (sym.val == "begin") {
        emitCode(OpCode::MAKE_NIL);
        visitEach(sExprs.rest, [&](const auto &sExpr) {
          stackOffset += 1;
          this->compileStmt(sExpr);
        });
        return;
      } else if (sym.val == "define") {
        compileDef(sExpr);
        return;
      } else if (sym.val == "defmacro") {
        execDefMacro(sExpr);
        return;
      } else if (vm.isMacro(sym)) {
        compileStmt(execMacro(sExpr));
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
  updateCurSrcLoc(sExpr);
  const auto &sExprs = cast<SExprs>(sExpr);
  if (isa<Sym>(sExprs.first)) {
    const auto &sym = cast<Sym>(sExprs.first);
    if (sym.val == "begin") {
      emitCode(OpCode::MAKE_NIL);
      visitEach(sExprs.rest, [&](const auto &sExpr) {
        emitCode(OpCode::POP_TOP);
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
      const auto [row, col] = curSrcLoc;
      throw error::SyntaxError(
          "Invalid syntax for define: cannot use define as an expression",
          source[row - 1], row, col);
    } else if (vm.isMacro(sym)) {
      compileExpr(execMacro(sExpr));
      return;
    }
  }
  compileCall(sExprs);
}

void Compiler::compileLambda(const SExpr &sExpr) {
  try {
    const auto &lamArgNames = cast<SExprs>(at(lambdaArgPos, sExpr)).first;
    const auto &lamBody = cast<SExprs>(at(lambdaBodyPos, sExpr));

    Compiler compiler(source, srcMap, lamArgNames, lamBody, *this, vm);
    const auto &function = compiler.compile();

    cast<Nil>(last(sExpr));

    emitCode(OpCode::MAKE_CLOSURE, emitConst(function));

    for (const auto &upValue : compiler.upValues) {
      emitCode(upValue.isLocal ? 1 : 0, upValue.idx);
    }
  } catch (error::TypeError &te) {
    handleSyntaxError(lambdaGrammar, te.expected, te.actual);
  }
}

void Compiler::compileCall(const SExprs &sExprs) {
  compileExpr(sExprs.first);
  const auto argc = visitEach(
      sExprs.rest, [&](const auto &sExpr) { this->compileExpr(sExpr); });

  try {
    cast<Nil>(last(sExprs));
  } catch (error::TypeError &te) {
    handleSyntaxError(callGrammar, te.expected, te.actual);
  }
  emitCode(OpCode::CALL, argc);
}

void Compiler::compileAtom(const Atom &atom) {
  if (isa<Sym>(atom)) {
    compileSym(cast<Sym>(atom));
    return;
  }
  if (isa<Nil>(atom)) {
    throw error::SyntaxError("Expected a non-empty list.",
                             source[curSrcLoc.row - 1], curSrcLoc.row,
                             curSrcLoc.col);
  }
  emitCode(OpCode::LOAD_CONST, emitConst(atom));
}

void Compiler::compileSym(const Sym &sym) {
  if (const auto idx = resolveLocal(sym); idx.has_value()) {
    emitCode(OpCode::LOAD_STACK, (uint8_t)locals[*idx].stackOffset);
    return;
  }
  if (const auto idx = resolveUpvalue(*this, sym); idx.has_value()) {
    emitCode(OpCode::LOAD_UPVALUE, (uint8_t)*idx);
    return;
  }
  emitCode(OpCode::LOAD_SYM, emitConst(sym));
}

void Compiler::compileQuote(const SExpr &sExpr) {
  try {
    const auto &expr = cast<SExprs>(at(quoteArgPos, sExpr)).first;
    emitCode(OpCode::LOAD_CONST, emitConst(expr));
    cast<Nil>(at(quoteNilPos, sExpr));
  } catch (error::TypeError &te) {
    handleSyntaxError(quoteGrammar, te.expected, te.actual);
  }
}

void Compiler::compileDef(const SExpr &sExpr) {
  try {
    const auto &sym = cast<Sym>(cast<SExprs>(at(defSymPos, sExpr)).first);
    const auto &expr = cast<SExprs>(at(defSExprPos, sExpr)).first;

    compileExpr(expr);

    cast<Nil>(at(defNilPos, sExpr));

    if (enclosing.has_value()) {
      locals.push_back({sym, stackOffset, false});
    } else {
      emitCode(OpCode::DEF_SYM, emitConst(sym));
    }
  } catch (error::TypeError &te) {
    handleSyntaxError(defGrammar, te.expected, te.actual);
  }
}

void Compiler::execDefMacro(const SExpr &sExpr) {
  if (enclosing.has_value()) {
    const auto [row, col] = curSrcLoc;
    throw error::SyntaxError(
        "Invalid syntax for define-macro: must define macros in top level",
        source[row - 1], row, col);
  }
  try {
    const auto &sym = cast<Sym>(cast<SExprs>(at(defMacroSymPos, sExpr)).first);
    const auto &argNames = cast<SExprs>(at(defMacroArgPos, sExpr)).first;
    const auto &body = cast<SExprs>(at(defMacroBodyPos, sExpr));

    Compiler compiler(source, srcMap, argNames, body, *this, vm);
    const auto &function = compiler.compile();

    Code def;

    def.pushCode(OpCode::MAKE_CLOSURE, curSrcLoc.row);
    def.pushCode(def.pushConst(function));

    for (const auto &upValue : compiler.upValues) {
      def.pushCode(upValue.isLocal ? 1 : 0);
      def.pushCode(upValue.idx);
    }

    def.pushCode(OpCode::DEF_SYM, curSrcLoc.row);
    def.pushCode(def.pushConst(sym));
    def.pushCode(OpCode::RETURN, curSrcLoc.row);

    vm.eval(vm.freeStore.alloc<Prototype>(0, 0, false, def));
    vm.regMacro(sym);

    cast<Nil>(last(sExpr));
    emitCode(OpCode::MAKE_NIL);
  } catch (error::TypeError &te) {
    handleSyntaxError(defMacroGrammar, te.expected, te.actual);
  }
}

void Compiler::compileSet(const SExpr &sExpr) {
  try {
    const auto &sym = cast<Sym>(cast<SExprs>(at(setSymPos, sExpr)).first);
    const auto &expr = cast<SExprs>(at(setSExprPos, sExpr)).first;

    compileExpr(expr);

    cast<Nil>(at(setNilPos, sExpr));

    if (const auto idx = resolveLocal(sym); idx.has_value()) {
      emitCode(OpCode::SET_STACK, locals[*idx].stackOffset);
      return;
    }
    if (const auto idx = resolveUpvalue(*this, sym); idx.has_value()) {
      emitCode(OpCode::SET_UPVALUE, (uint8_t)*idx);
      return;
    }
    emitCode(OpCode::SET_SYM, emitConst(sym));
  } catch (error::TypeError &te) {
    handleSyntaxError(setGrammar, te.expected, te.actual);
  }
}

void Compiler::compileIf(const SExpr &sExpr) {
  try {
    const auto &test = cast<SExprs>(at(ifTestPos, sExpr)).first;
    compileExpr(test);
    const auto jifIdx =
        emitCode(OpCode::POP_JUMP_IF_FALSE, UINT8_MAX, UINT8_MAX) + 1;

    const auto &conseq = cast<SExprs>(at(ifConseqPos, sExpr)).first;
    compileExpr(conseq);
    const auto jIdx = emitCode(OpCode::JUMP, UINT8_MAX, UINT8_MAX) + 1;
    patchJump(jifIdx);

    const auto &alt = cast<SExprs>(at(ifAltPos, sExpr)).first;
    compileExpr(alt);
    patchJump(jIdx);

    cast<Nil>(at(ifNilPos, sExpr));
  } catch (error::TypeError &te) {
    handleSyntaxError(ifGrammar, te.expected, te.actual);
  }
}

void Compiler::compileRet() {
  try {
    cast<Nil>(last(body));
  } catch (error::TypeError &te) {
    handleSyntaxError(lambdaGrammar, te.expected, te.actual);
  }

  emitCode(OpCode::SET_STACK, 0);

  for (const auto &local : locals) {
    if (local.isCaptured) {
      emitCode(OpCode::CLOSE_UPVALUE, local.stackOffset);
    }
  }

  emitCode(OpCode::POP, stackOffset);
  emitCode(OpCode::RETURN);
}

const SExpr &Compiler::execMacro(const SExpr &sExpr) {
  Code fexpr;

  const auto &sExprs = cast<SExprs>(sExpr);

  fexpr.pushCode(OpCode::LOAD_SYM, curSrcLoc.row);
  fexpr.pushCode(fexpr.pushConst(sExprs.first));

  const auto argc = visitEach(sExprs.rest, [&](const auto &sExpr) {
    fexpr.pushCode(OpCode::LOAD_CONST, curSrcLoc.row);
    fexpr.pushCode(fexpr.pushConst(sExpr));
  });

  fexpr.pushCode(OpCode::CALL, curSrcLoc.row);
  fexpr.pushCode(argc);
  fexpr.pushCode(OpCode::RETURN, curSrcLoc.row);

  const auto &res = vm.eval(vm.freeStore.alloc<Prototype>(0, 0, false, fexpr));

  traverse(res, [&](const auto &sExpr) { srcMap.insert({&sExpr, curSrcLoc}); });

  return res;
}

void Compiler::handleSyntaxError(const std::string grammar,
                                 const std::string expected,
                                 const SExpr &actual) {
  std::stringstream ss;
  ss << "Invalid syntax for " << grammar << "." << std::endl
     << "Expected " << expected << ", but got " << actual << ".";
  const auto [row, col] = curSrcLoc;
  throw SyntaxError(ss.str(), source[row - 1], row, col);
}

Compiler::Compiler(std::vector<std::string> source, VM &vm)
    : vm(vm), source(source), curSrcLoc({1, 0}),
      argNames(vm.freeStore.alloc<Nil>()), arity(0), variadic(false),
      body(parse()), stackOffset(1) {}

const Prototype &Compiler::compile() {
  if (variadic) {
    emitCode(OpCode::MAKE_LIST, arity + 1);
  }

  emitCode(OpCode::MAKE_NIL);
  visitEach(body, [&](const auto &sExpr) {
    stackOffset += 1;
    this->compileStmt(sExpr);
  });

  compileRet();

  return vm.freeStore.alloc<Prototype>(upValues.size(), arity, variadic, code);
}

void Compiler::verifyLex(const std::string &line, const unsigned int curSrcLoc,
                         unsigned int &openParen, unsigned int &closedParen) {
  auto tokens = tokenize(line, curSrcLoc);
  for (auto it = tokens.cbegin(); it != tokens.cend(); ++it) {
    if (openParen == closedParen && it->str == ")") {
      handleUnexpectedToken(*it, line);
    }
    if (it->str == "(") {
      openParen += 1;
    } else if (it->str == ")") {
      closedParen += 1;
    }
  }
}
