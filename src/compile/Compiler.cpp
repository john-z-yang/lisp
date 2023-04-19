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

Compiler::Compiler(std::vector<std::string> lines)
    : enclosing(nullptr), arg(std::make_shared<NilAtom>()),
      body(parse(lines, sourceLoc)), function(std::make_shared<FnAtom>(0)),
      scopeDepth(0) {}

std::shared_ptr<FnAtom> Compiler::compile() {
  if (function->arity == -1) {
    getCode().pushCode(OpCode::MAKE_VAR_ARGS);
  }

  compile(body);
  getCode().pushCode(OpCode::RETURN);

  function->numUpVals = upValues.size();
  return function;
}

Compiler::Compiler(std::shared_ptr<SExpr> arg, std::shared_ptr<SExpr> body,
                   unsigned int scopeDepth, SourceLoc sourceLoc,
                   Compiler *enclosing)
    : enclosing(enclosing), sourceLoc(sourceLoc), arg(arg), body(body),
      function(std::make_shared<FnAtom>(0)), scopeDepth(scopeDepth) {

  if (const auto argNames = std::dynamic_pointer_cast<SExprs>(arg)) {
    visitEach(argNames, [&](std::shared_ptr<SExpr> sExpr) {
      auto sym = cast<SymAtom>(sExpr);
      locals.push_back({sym, scopeDepth});
    });

    function = std::make_unique<FnAtom>(locals.size());
  } else if (const auto argName = std::dynamic_pointer_cast<SymAtom>(arg)) {
    locals.push_back({argName, scopeDepth});

    function = std::make_unique<FnAtom>(-1);
  }
}

void Compiler::compile(std::shared_ptr<SExpr> sExpr) {
  if (isa<NilAtom>(*sExpr) || isa<IntAtom>(*sExpr) || isa<BoolAtom>(*sExpr) ||
      isa<StringAtom>(*sExpr)) {
    const auto lineNum = sourceLoc[sExpr];
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
  compile(sExprs->first);
  const auto argc = visitEach(sExprs->rest, [&](std::shared_ptr<SExpr> sExpr) {
    this->compile(sExpr);
  });

  const auto lineNum = sourceLoc[sExprs->first];
  getCode().pushCode(OpCode::CALL, lineNum);
  getCode().pushCode(argc, lineNum);
}

void Compiler::compileSym(std::shared_ptr<SymAtom> sym) {
  const auto lineNum = sourceLoc[sym];

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

  const auto lineNum = sourceLoc[sExpr];
  getCode().pushCode(OpCode::LOAD_CONST, lineNum);
  getCode().pushCode(getCode().pushConst(expr), lineNum);
}

void Compiler::compileDef(std::shared_ptr<SExpr> sExpr) {
  try {
    const auto sym = cast<SymAtom>(cast<SExprs>(at(defSymPos, sExpr))->first);
    const auto expr = cast<SExprs>(at(defSExprPos, sExpr))->first;
    cast<NilAtom>(at(defNilPos, sExpr));

    compile(expr);

    const auto lineNum = sourceLoc[sExpr];
    getCode().pushCode(OpCode::DEF_SYM, lineNum);
    getCode().pushCode(getCode().pushConst(sym), lineNum);
  } catch (TypeError &te) {
    handleSyntaxError(defGrammar, sExpr);
  }
}

void Compiler::compileSet(std::shared_ptr<SExpr> sExpr) {
  try {
    const auto sym = cast<SymAtom>(cast<SExprs>(at(setSymPos, sExpr))->first);
    const auto expr = cast<SExprs>(at(setSExprPos, sExpr))->first;
    cast<NilAtom>(at(setNilPos, sExpr));

    compile(expr);

    const auto lineNum = sourceLoc[sExpr];

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
    handleSyntaxError(setGrammar, sExpr);
  }
}

void Compiler::compileIf(std::shared_ptr<SExpr> sExpr) {
  try {
    const auto test = cast<SExprs>(at(ifTestPos, sExpr))->first;
    const auto conseq = cast<SExprs>(at(ifConseqPos, sExpr))->first;
    const auto alt = cast<SExprs>(at(ifAltPos, sExpr))->first;
    cast<NilAtom>(at(ifNilPos, sExpr));

    compile(test);

    const auto testLoc = sourceLoc[test];
    const auto jifIdx =
        getCode().pushCode(OpCode::POP_JUMP_IF_FALSE, testLoc) + 1;
    getCode().pushCode(UINT8_MAX, testLoc);
    getCode().pushCode(UINT8_MAX, testLoc);

    compile(conseq);

    const auto conseqLoc = sourceLoc[conseq];
    const auto jIdx = getCode().pushCode(OpCode::JUMP, conseqLoc) + 1;
    getCode().pushCode(UINT8_MAX, conseqLoc);
    getCode().pushCode(UINT8_MAX, conseqLoc);
    getCode().patchJump(jifIdx);

    compile(alt);

    getCode().patchJump(jIdx);
  } catch (TypeError &te) {
    handleSyntaxError(ifGrammar, sExpr);
  }
}

void Compiler::compileLambda(std::shared_ptr<SExpr> sExpr) {
  try {
    const auto argNames = cast<SExprs>(at(lambdaArgPos, sExpr))->first;
    const auto body = cast<SExprs>(at(lambdaBodyPos, sExpr))->first;
    cast<NilAtom>(at(lambdaNilPos, sExpr));

    Compiler compiler(argNames, body, scopeDepth + 1, sourceLoc, this);
    const auto function = compiler.compile();

    const auto lineNum = sourceLoc[sExpr];
    getCode().pushCode(OpCode::MAKE_CLOSURE, lineNum);
    getCode().pushCode(getCode().pushConst(function), lineNum);
    for (const auto &upValue : compiler.upValues) {
      getCode().pushCode(upValue.isLocal ? 1 : 0);
      getCode().pushCode(upValue.idx);
    }
  } catch (TypeError &te) {
    handleSyntaxError(lambdaGrammar, sExpr);
  }
}

void Compiler::handleSyntaxError(std::string expected,
                                 std::shared_ptr<SExpr> actual) {
  std::stringstream ss;
  ss << "Expected \"" << expected << "\", but got \"" << *actual << "\".";
  std::cout << ss.str();
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
  return std::distance(locals.begin(), it.base());
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

void Compiler::beginScope() { scopeDepth += 1; }

void Compiler::endScope() { setScope(scopeDepth - 1); }

void Compiler::setScope(unsigned int scope) {
  scopeDepth = scope;
  while (locals.size() && locals.back().depth > scopeDepth) {
    locals.pop_back();
    getCode().pushCode(OpCode::POP_TOP);
  }
}
