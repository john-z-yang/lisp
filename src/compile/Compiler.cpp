#include "Compiler.hpp"
#include "../code/OpCode.hpp"
#include "../error/SyntaxError.hpp"
#include "../runtime/VM.hpp"
#include "../sexpr/Cast.cpp"
#include "../sexpr/String.hpp"
#include "Grammar.hpp"
#include "SrcLoc.hpp"
#include <optional>
#include <ostream>
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

std::vector<Token>
Compiler::tokenize(std::string line, const unsigned int row) {
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
  auto it = tokens.cbegin();
  return cast<SExprs>(parseLists(it, tokens.cend()));
}

const SExpr *Compiler::parseLists(TokenIter &it, const TokenIter &end) {
  if (it == end) {
    return vm.heap.alloc<Nil>();
  }
  const auto [row, col] = it->srcLoc;
  const auto cur = parseList(it, end);
  const auto sexprs = vm.heap.alloc<SExprs>(cur, parseLists(it, end));
  srcMap[sexprs] = {row, col};
  return sexprs;
}

const SExpr *Compiler::parseList(TokenIter &it, const TokenIter &end) {
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

const SExpr *Compiler::parseElem(TokenIter &it, const TokenIter &end) {
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

const SExpr *Compiler::parseSexprs(TokenIter &it, const TokenIter &end) {
  auto token = *it;
  const auto first = parseList(it, end);
  if (it->str == ".") {
    it += 1;
    const auto rest = parseList(it, end);
    if (it == end) {
      handleTypeError(dotGrammer, "datum", rest);
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

const SExpr *Compiler::parseAtom(Token token) {
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

void Compiler::handleUnexpectedToken(
    const Token &token, const std::string &line
) {
  std::stringstream ss;
  ss << "Unexpected \"" << token.str << "\".";
  throw SyntaxError(ss.str(), line, token.srcLoc.row, token.srcLoc.col);
}

Compiler::Compiler(
    const std::vector<std::string> source,
    SrcMap sourceLoc,
    const SExpr *param,
    const SExprs *body,
    Compiler &enclosing,
    VM &vm
)
    : vm(vm),
      enclosing(enclosing),
      source(source),
      srcMap(sourceLoc),
      curSrcLoc({srcMap[param].row, srcMap[param].col}),
      param(param),
      arity(countArity()),
      variadic(isVariadic()),
      body(body),
      stackOffset(1) {

  if (isa<SExprs>(param)) {
    visitEach(cast<SExprs>(param), [this](const auto sExpr) {
      const auto sym = cast<Sym>(sExpr);
      locals.push_back({sym, stackOffset, false});
      stackOffset += 1;
    });
  }

  const auto lastParam = last(param);

  if (isa<Sym>(lastParam)) {
    locals.push_back({cast<Sym>(lastParam), stackOffset, false});
    stackOffset += 1;
  }
}

void Compiler::updateCurSrcLoc(const sexpr::SExprs *sExpr) {
  curSrcLoc = srcMap[sExpr];
}

std::optional<const std::size_t> Compiler::resolveLocal(const Sym *sym) {
  auto it =
      std::find_if(locals.rbegin(), locals.rend(), [&sym](const auto &local) {
        return *local.symbol == *sym;
      });
  if (it == locals.rend()) {
    return std::nullopt;
  }
  return std::distance(begin(locals), it.base()) - 1;
}

std::optional<const std::size_t>
Compiler::resolveUpvalue(Compiler &caller, const Sym *sym) {
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
  if (auto it = std::find_if(
          upValues.cbegin(),
          upValues.cend(),
          [idx, isLocal](const auto upValue) {
            return upValue.idx == idx && upValue.isLocal == isLocal;
          }
      );
      it != upValues.end()) {
    return std::distance(upValues.cbegin(), it);
  }
  upValues.push_back({idx, isLocal});
  return upValues.size() - 1;
}

bool Compiler::isVariadic() { return isa<Sym>(last(param)); }

uint8_t Compiler::countArity() {
  if (isa<Nil>(param) || isa<Sym>(param)) {
    return 0;
  }
  return visitEach(cast<SExprs>(param), [](const auto) {});
}

code::InstrPtr Compiler::emitConst(const sexpr::SExpr *sExpr) {
  return code.pushConst(sExpr);
}

void Compiler::patchJump(const code::InstrPtr idx) { code.patchJump(idx); }

const SExpr *Compiler::last(const SExpr *sExpr) {
  if (isa<Atom>(sExpr)) {
    return sExpr;
  }
  const auto sExprs = cast<SExprs>(sExpr);
  updateCurSrcLoc(sExprs);
  return last(sExprs->rest);
}

unsigned int Compiler::visitEach(const SExpr *sExpr, Visitor visitor) {
  if (isa<Atom>(sExpr)) {
    return 0;
  }
  const auto sExprs = cast<SExprs>(sExpr);
  updateCurSrcLoc(sExprs);
  visitor(sExprs->first);
  return 1 + visitEach(sExprs->rest, visitor);
}

void Compiler::traverse(const SExpr *sExpr, Visitor visitor) {
  if (isa<SExprs>(sExpr)) {
    const auto sexprs = cast<SExprs>(sExpr);
    traverse(sexprs->first, visitor);
    traverse(sexprs->rest, visitor);
  }
  visitor(sExpr);
}

void Compiler::compileStmts(const SExpr *sExpr) {
  emitCode(OpCode::MAKE_NIL);
  visitEach(sExpr, [this](const auto sExpr) {
    stackOffset += 1;
    compileStmt(sExpr);
  });
}

void Compiler::compileExprs(const SExpr *sExpr) {
  emitCode(OpCode::MAKE_NIL);
  visitEach(sExpr, [this](const auto sExpr) {
    emitCode(OpCode::POP_TOP);
    compileExpr(sExpr);
  });
}

void Compiler::compileStmt(const SExpr *sExpr) {
  if (matchForm(
          sExpr,
          {
              {&DEFINE_SYM, [this](const auto &matched) { emitDef(matched); }},
              {&DEFMACRO_SYM,
               [this](const auto &matched) { execDefMacro(matched); }},
              {&BEGIN_SYM,
               [this](const auto &matched) { compileStmts(matched.get()); }},
          },
          [this, &sExpr](const auto &sym, const auto) {
            if (vm.env.isMacro(sym.get())) {
              compileStmt(execMacro(sExpr));
              return;
            }
            compileExpr(sExpr);
          }
      )) {
    return;
  }
  compileExpr(sExpr);
}

void Compiler::compileExpr(const SExpr *sExpr) {
  if (matchForm(
          sExpr,
          {{&DEFINE_SYM, [this](const auto &) { handleInvalidDef(); }},
           {&DEFMACRO_SYM, [this](const auto &) { handleInvalidDef(); }},
           {&QUOTE_SYM, [this](const auto &matched) { emitQuote(matched); }},
           {&SET_SYM, [this](const auto &matched) { emitSet(matched); }},
           {&IF_SYM, [this](const auto &matched) { emitIf(matched); }},
           {&LAMBDA_SYM, [this](const auto &matched) { emitLambda(matched); }},
           {&BEGIN_SYM,
            [this](const auto &matched) { compileExprs(matched.get()); }}},
          [this, &sExpr](const auto sym, const auto) {
            if (vm.env.isMacro(sym.get())) {
              compileExpr(execMacro(sExpr));
              return;
            }
            compileCall(cast<SExprs>(sExpr));
          }
      )) {
    return;
  };
  if (isa<Atom>(sExpr)) {
    compileAtom(cast<Atom>(sExpr));
    return;
  }
  compileCall(cast<SExprs>(sExpr));
}

void Compiler::compileAtom(const Atom *atom) {
  if (isa<Nil>(atom)) {
    throw error::SyntaxError(
        "Expected a non-empty list.",
        source[curSrcLoc.row - 1],
        curSrcLoc.row,
        curSrcLoc.col
    );
  }
  if (isa<Sym>(atom)) {
    emitSym(cast<Sym>(atom));
    return;
  }
  emitCode(OpCode::LOAD_CONST, emitConst(atom));
}

void Compiler::compileCall(const SExprs *sExprs) {
  compileExpr(sExprs->first);
  const auto argc = visitEach(sExprs->rest, [this](const auto &sExpr) {
    compileExpr(sExpr);
  });

  try {
    cast<Nil>(last(sExprs));
  } catch (error::TypeError &te) {
    handleTypeError(callGrammar, te.expected, te.actual);
  }
  emitCode(OpCode::CALL, argc);
}

void Compiler::emitLambda(const MatchedSExpr<sexpr::SExpr> matched) {
  try {
    const auto &[lambdaParam, lambdaBody] = unpackPartial<SExpr>(matched.get());

    if (isa<SExprs>(lambdaParam.get())) {
      visitEach(lambdaParam.get(), [](const auto &argName) {
        assertType<Sym>(argName);
      });
      assertType<Sym, Nil>(last(lambdaParam.get()));
    }

    Compiler compiler(
        source,
        srcMap,
        lambdaParam.get(),
        cast<SExprs>(lambdaBody.get()),
        *this,
        vm
    );
    const auto function = compiler.compile();

    emitCode(OpCode::MAKE_CLOSURE, emitConst(function));
    for (const auto &upValue : compiler.upValues) {
      emitCode(upValue.isLocal ? 1 : 0, upValue.idx);
    }
  } catch (error::TypeError &te) {
    handleTypeError(lambdaGrammar, te.expected, te.actual);
  }
}

void Compiler::emitSym(const sexpr::Sym *sym) {
  if (vm.env.isNatFn(sym)) {
    emitCode(OpCode::LOAD_CONST, emitConst(vm.env.load(sym)));
    return;
  }
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

void Compiler::emitQuote(const MatchedSExpr<sexpr::SExpr> matched) {
  try {
    const auto &[expr] = unpack<SExpr>(matched.get());

    emitCode(OpCode::LOAD_CONST, emitConst(expr.get()));
  } catch (error::TypeError &te) {
    handleTypeError(quoteGrammar, te.expected, te.actual);
  }
}

void Compiler::emitDef(const MatchedSExpr<sexpr::SExpr> matched) {
  try {
    const auto &[sym, expr] = unpack<Sym, SExpr>(matched.get());

    compileExpr(expr.get());
    if (enclosing.has_value()) {
      locals.push_back({sym.get(), stackOffset, false});
    } else {
      emitCode(OpCode::DEF_SYM, emitConst(sym.get()));
    }
  } catch (error::TypeError &te) {
    handleTypeError(defGrammar, te.expected, te.actual);
  }
}

void Compiler::execDefMacro(const MatchedSExpr<sexpr::SExpr> matched) {
  if (enclosing.has_value()) {
    const auto [row, col] = curSrcLoc;
    throw error::SyntaxError(
        "Invalid syntax for define-macro: must define macros in top level",
        source[row - 1],
        row,
        col
    );
  }
  try {
    const auto &[macroSym, macroArgNames, macroBody] =
        unpackPartial<Sym, SExpr>(matched.get());

    Compiler compiler(
        source,
        srcMap,
        macroArgNames.get(),
        cast<SExprs>(macroBody.get()),
        *this,
        vm
    );
    const auto &function = compiler.compile();

    Code def;

    def.pushCode(OpCode::MAKE_CLOSURE, curSrcLoc.row);
    def.pushCode(def.pushConst(function));

    for (const auto &upValue : compiler.upValues) {
      def.pushCode(upValue.isLocal ? 1 : 0);
      def.pushCode(upValue.idx);
    }

    def.pushCode(OpCode::DEF_MACRO, curSrcLoc.row);
    def.pushCode(def.pushConst(macroSym.get()));
    def.pushCode(OpCode::RETURN, curSrcLoc.row);

    vm.eval(vm.heap.alloc<Prototype>(0, 0, false, def), true);

    emitCode(OpCode::MAKE_NIL);
    assertType<Nil>(last(macroBody.get()));
  } catch (error::TypeError &te) {
    handleTypeError(defMacroGrammar, te.expected, te.actual);
  }
}

void Compiler::emitSet(const MatchedSExpr<sexpr::SExpr> matched) {
  try {
    const auto &[sym, expr] = unpack<Sym, SExpr>(matched.get());

    compileExpr(expr.get());
    if (const auto idx = resolveLocal(sym.get()); idx.has_value()) {
      emitCode(OpCode::SET_STACK, locals[*idx].stackOffset);
      return;
    }
    if (const auto idx = resolveUpvalue(*this, sym.get()); idx.has_value()) {
      emitCode(OpCode::SET_UPVALUE, (uint8_t)*idx);
      return;
    }
    emitCode(OpCode::SET_SYM, emitConst(sym.get()));
  } catch (error::TypeError &te) {
    handleTypeError(setGrammar, te.expected, te.actual);
  }
}

void Compiler::emitIf(const MatchedSExpr<sexpr::SExpr> matched) {
  try {
    const auto &[test, conseq, alt] =
        unpack<SExpr, SExpr, SExpr>(matched.get());

    compileExpr(test.get());
    const auto jifIdx =
        emitCode(OpCode::POP_JUMP_IF_FALSE, UINT8_MAX, UINT8_MAX) + 1;

    compileExpr(conseq.get());
    const auto jIdx = emitCode(OpCode::JUMP, UINT8_MAX, UINT8_MAX) + 1;
    patchJump(jifIdx);

    compileExpr(alt.get());
    patchJump(jIdx);
  } catch (error::TypeError &te) {
    handleTypeError(ifGrammar, te.expected, te.actual);
  }
}

void Compiler::emitRet() {
  try {
    cast<Nil>(last(body));
  } catch (error::TypeError &te) {
    handleTypeError(lambdaGrammar, te.expected, te.actual);
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

const SExpr *Compiler::execMacro(const SExpr *sExpr) {
  Code fExpr;

  const auto sExprs = cast<SExprs>(sExpr);

  fExpr.pushCode(OpCode::LOAD_SYM, curSrcLoc.row);
  fExpr.pushCode(fExpr.pushConst(sExprs->first));

  const auto argc = visitEach(sExprs->rest, [this, &fExpr](const auto &sExpr) {
    fExpr.pushCode(OpCode::LOAD_CONST, curSrcLoc.row);
    fExpr.pushCode(fExpr.pushConst(sExpr));
  });

  fExpr.pushCode(OpCode::CALL, curSrcLoc.row);
  fExpr.pushCode(argc);
  fExpr.pushCode(OpCode::RETURN, curSrcLoc.row);

  const auto res = vm.eval(vm.heap.alloc<Prototype>(0, 0, false, fExpr), true);

  traverse(res, [this](const auto &sExpr) {
    srcMap.insert({sExpr, curSrcLoc});
  });

  return res;
}

void Compiler::handleInvalidDef() {
  const auto [row, col] = curSrcLoc;
  throw error::SyntaxError(
      "Invalid syntax for define: cannot use define as an "
      "expression",
      source[row - 1],
      row,
      col
  );
}

void Compiler::handleTypeError(
    const std::string grammar, const std::string expected, const SExpr *actual
) {
  std::stringstream ss;
  ss << "Invalid syntax for " << grammar << "." << std::endl
     << "Expected " << expected << ", but got " << actual << ".";
  const auto [row, col] = curSrcLoc;
  throw SyntaxError(ss.str(), source[row - 1], row, col);
}

Compiler::Compiler(std::vector<std::string> source, VM &vm)
    : vm(vm),
      source(source),
      curSrcLoc({1, 0}),
      param(vm.heap.alloc<Nil>()),
      arity(0),
      variadic(false),
      body(parse()),
      stackOffset(1) {}

const Prototype *Compiler::compile() {
  if (variadic) {
    emitCode(OpCode::MAKE_LIST, arity + 1);
  }

  compileStmts(body);
  emitRet();

  return vm.heap.alloc<Prototype>(upValues.size(), arity, variadic, code);
}

void Compiler::verifyLex(
    const std::string &line,
    const unsigned int curSrcLoc,
    unsigned int &openParen,
    unsigned int &closedParen
) {
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
