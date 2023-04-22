#include "Compiler.hpp"
#include "../code/Code.hpp"
#include "../code/OpCode.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/StringAtom.hpp"
#include "../sexpr/SymAtom.hpp"
#include "../sexpr/TypeError.hpp"
#include "../sexpr/cast.cpp"
#include "SyntaxError.hpp"
#include "grammar.hpp"
#include "parse.hpp"
#include <algorithm>
#include <cstddef>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

Compiler::Compiler(std::vector<std::string> source)
    : source(source), enclosing(nullptr), arg(std::make_shared<NilAtom>()),
      body(parse(source, sourceLoc)), function(std::make_shared<FnAtom>(0)),
      stackOffset(0) {}

std::shared_ptr<FnAtom> Compiler::compile() {
  if (function->arity == -1) {
    getCode().pushCode(OpCode::MAKE_VAR_ARGS);
  }

  compile(body);
  getCode().pushCode(OpCode::RETURN);

  function->numUpVals = upValues.size();
  return function;
}

Compiler::Compiler(const std::vector<std::string> source, SourceLoc sourceLoc,
                   std::shared_ptr<SExpr> arg, std::shared_ptr<SExpr> body,
                   Compiler *enclosing)
    : source(source), sourceLoc(sourceLoc), enclosing(enclosing), arg(arg),
      body(body), function(std::make_shared<FnAtom>(0)), stackOffset(0) {
  locals.push_back({std::make_unique<SymAtom>(""), stackOffset});
  stackOffset += 1;

  if (const auto argNames = std::dynamic_pointer_cast<SExprs>(arg)) {
    visitEach(argNames, [&](std::shared_ptr<SExpr> sExpr) {
      auto sym = cast<SymAtom>(sExpr);
      locals.push_back({sym, stackOffset});
      stackOffset += 1;
    });

    function = std::make_unique<FnAtom>(locals.size() - 1);
  } else if (const auto argName = std::dynamic_pointer_cast<SymAtom>(arg)) {
    locals.push_back({argName, stackOffset});
    stackOffset += 1;

    function = std::make_unique<FnAtom>(-1);
  } else if (!isa<NilAtom>(*arg)) {
    handleSyntaxError(lambdaGrammar, NilAtom::typeName, arg);
  }
}

void Compiler::compile(std::shared_ptr<SExpr> sExpr) {
  if (isa<NilAtom>(*sExpr) || isa<IntAtom>(*sExpr) || isa<BoolAtom>(*sExpr) ||
      isa<StringAtom>(*sExpr)) {
    const auto lineNum = std::get<0>(sourceLoc[sExpr]);
    getCode().pushCode(OpCode::LOAD_CONST, lineNum);
    getCode().pushCode(getCode().pushConst(sExpr), lineNum);
    return;
  } else if (const auto sym = std::dynamic_pointer_cast<SymAtom>(sExpr)) {
    compileSym(sym);
    return;
  }
  const auto sExprs = cast<SExprs>(sExpr);
  if (const auto sym = std::dynamic_pointer_cast<SymAtom>(sExprs->first)) {
    if (sym->val == "quote") {
      compileQuote(sExpr);
      return;
    }
    if (sym->val == "define") {
      compileDef(sExpr);
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
    }
  }
  compileCall(sExprs);
}

void Compiler::compileSym(std::shared_ptr<SymAtom> sym) {
  const auto lineNum = std::get<0>(sourceLoc[sym]);

  if (const auto idx = resolveLocal(sym); idx != -1) {
    getCode().pushCode(OpCode::LOAD_STACK, lineNum);
    getCode().pushCode((uint8_t)idx, lineNum);
    return;
  }
  if (const auto idx = resolveUpvalue(*this, sym); idx != -1) {
    getCode().pushCode(OpCode::LOAD_UPVALUE, lineNum);
    getCode().pushCode((uint8_t)idx, lineNum);
    return;
  }
  getCode().pushCode(OpCode::LOAD_SYM, lineNum);
  getCode().pushCode(getCode().pushConst(sym), lineNum);
}

void Compiler::compileQuote(std::shared_ptr<SExpr> sExpr) {
  const auto expr = cast<SExprs>(at(quoteArgPos, sExpr))->first;
  cast<NilAtom>(at(quoteNilPos, sExpr));

  const auto lineNum = std::get<0>(sourceLoc[sExpr]);
  getCode().pushCode(OpCode::LOAD_CONST, lineNum);
  getCode().pushCode(getCode().pushConst(expr), lineNum);
}

void Compiler::compileDef(std::shared_ptr<SExpr> sExpr) {
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
      getCode().pushCode(OpCode::DEF_SYM, lineNum);
      getCode().pushCode(getCode().pushConst(sym), lineNum);
    } else {
      locals.push_back({sym, stackOffset});
    }
  } catch (TypeError &te) {
    handleSyntaxError(defGrammar, te.expected, te.actual);
  }
}

void Compiler::compileSet(std::shared_ptr<SExpr> sExpr) {
  try {
    const auto sym = cast<SymAtom>(cast<SExprs>(at(setSymPos, sExpr))->first);
    const auto expr = cast<SExprs>(at(setSExprPos, sExpr))->first;
    cast<NilAtom>(at(setNilPos, sExpr));

    compile(expr);

    const auto lineNum = std::get<0>(sourceLoc[sExpr]);

    if (const auto idx = resolveLocal(sym); idx != -1) {
      getCode().pushCode(OpCode::SET_STACK, lineNum);
      getCode().pushCode(idx, lineNum);
      return;
    }
    if (const auto idx = resolveUpvalue(*this, sym); idx != -1) {
      getCode().pushCode(OpCode::SET_UPVALUE, lineNum);
      getCode().pushCode((uint8_t)idx, lineNum);
      return;
    }
    getCode().pushCode(OpCode::SET_SYM, lineNum);
    getCode().pushCode(getCode().pushConst(sym), lineNum);
  } catch (TypeError &te) {
    handleSyntaxError(setGrammar, te.expected, te.actual);
  }
}

void Compiler::compileIf(std::shared_ptr<SExpr> sExpr) {
  try {
    const auto test = cast<SExprs>(at(ifTestPos, sExpr))->first;
    const auto conseq = cast<SExprs>(at(ifConseqPos, sExpr))->first;
    const auto alt = cast<SExprs>(at(ifAltPos, sExpr))->first;
    cast<NilAtom>(at(ifNilPos, sExpr));

    compile(test);

    const auto testLoc = std::get<0>(sourceLoc[test]);
    const auto jifIdx =
        getCode().pushCode(OpCode::POP_JUMP_IF_FALSE, testLoc) + 1;
    getCode().pushCode(UINT8_MAX, testLoc);
    getCode().pushCode(UINT8_MAX, testLoc);

    compile(conseq);

    const auto conseqLoc = std::get<0>(sourceLoc[conseq]);
    const auto jIdx = getCode().pushCode(OpCode::JUMP, conseqLoc) + 1;
    getCode().pushCode(UINT8_MAX, conseqLoc);
    getCode().pushCode(UINT8_MAX, conseqLoc);
    getCode().patchJump(jifIdx);

    compile(alt);

    getCode().patchJump(jIdx);
  } catch (TypeError &te) {
    handleSyntaxError(ifGrammar, te.expected, te.actual);
  }
}

void Compiler::compileLambda(std::shared_ptr<SExpr> sExpr) {
  try {
    const auto argNames = cast<SExprs>(at(lambdaArgPos, sExpr))->first;
    const auto body = cast<SExprs>(at(lambdaBodyPos, sExpr))->first;
    cast<NilAtom>(at(lambdaNilPos, sExpr));

    Compiler compiler(source, sourceLoc, argNames, body, this);
    const auto function = compiler.compile();

    const auto lineNum = std::get<0>(sourceLoc[sExpr]);
    getCode().pushCode(OpCode::MAKE_CLOSURE, lineNum);
    getCode().pushCode(getCode().pushConst(function), lineNum);
    for (const auto &upValue : compiler.upValues) {
      getCode().pushCode(upValue.isLocal ? 1 : 0);
      getCode().pushCode(upValue.idx);
    }
  } catch (TypeError &te) {
    handleSyntaxError(lambdaGrammar, te.expected, te.actual);
  }
}

void Compiler::compileCall(std::shared_ptr<SExprs> sExprs) {
  compile(sExprs->first);
  stackOffset += 1;
  const auto argc = visitEach(sExprs->rest, [&](std::shared_ptr<SExpr> sExpr) {
    this->compile(sExpr);
    stackOffset += 1;
  });

  const auto lineNum = std::get<0>(sourceLoc[sExprs->first]);
  getCode().pushCode(OpCode::CALL, lineNum);
  getCode().pushCode(argc, lineNum);
}

void Compiler::handleSyntaxError(const std::string grammar,
                                 const std::string expected,
                                 const std::shared_ptr<SExpr> actual) {
  std::stringstream ss;
  ss << "Invalid syntax for " << grammar << "." << std::endl
     << "Expected \"" << expected << "\", but got \"" << *actual << "\".";
  const auto [row, col] = sourceLoc[actual];
  throw SyntaxError(ss.str(), source[row - 1], row, col);
}

const unsigned int Compiler::visitEach(std::shared_ptr<SExpr> sExprs,
                                       Visitor visitor) {
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

std::shared_ptr<SExpr> Compiler::at(const unsigned int n,
                                    std::shared_ptr<SExpr> sExpr) {
  std::shared_ptr<SExpr> it = cast<SExprs>(sExpr);
  for (unsigned int i{0}; i < n; ++i) {
    it = cast<SExprs>(it)->rest;
  }
  return it;
}

Code &Compiler::getCode() { return function->code; }

int Compiler::resolveLocal(std::shared_ptr<SymAtom> sym) {
  auto it = std::find_if(locals.rbegin(), locals.rend(),
                         [&sym](Local local) { return *local.symbol == *sym; });
  if (it == locals.rend()) {
    return -1;
  }
  return it->stackOffset;
}

int Compiler::resolveUpvalue(Compiler &caller, std::shared_ptr<SymAtom> sym) {
  if (enclosing) {
    if (auto idx = enclosing->resolveLocal(sym); idx != -1) {
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
