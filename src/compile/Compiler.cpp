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
#include <algorithm>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>

std::shared_ptr<FunctionAtom> Compiler::compile() {
  compile(body);
  getCode().pushCode(OpCode::RETURN, -1);
  return function;
}

void Compiler::compile(std::shared_ptr<SExpr> sExpr) {
  if (isa<NilAtom>(*sExpr) || isa<IntAtom>(*sExpr) || isa<BoolAtom>(*sExpr) ||
      isa<StringAtom>(*sExpr)) {
    getCode().pushCode(OpCode::LOAD_CONST, -1);
    getCode().pushCode(getCode().pushConst(sExpr), -1);
    return;
  } else if (auto sym = std::dynamic_pointer_cast<SymAtom>(sExpr)) {
    compileSym(sym);
    return;
  }
  auto sExprs = cast<SExprs>(sExpr);
  if (auto sym = std::dynamic_pointer_cast<SymAtom>(sExprs->first)) {
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
    } else if (sym->val == "=") {
      visitEach(sExprs->rest,
                [&](std::shared_ptr<SExpr> sExpr) { this->compile(sExpr); });
      getCode().pushCode(OpCode::EQ, -1);
      return;
    } else if (sym->val == "+") {
      visitEach(sExprs->rest,
                [&](std::shared_ptr<SExpr> sExpr) { this->compile(sExpr); });
      getCode().pushCode(OpCode::ADD, -1);
      return;
    } else if (sym->val == "-") {
      auto numParams =
          visitEach(sExprs->rest, [&](std::shared_ptr<SExpr> sExpr) {
            this->compile(sExpr);
          });
      getCode().pushCode(numParams == 2 ? OpCode::SUB : OpCode::NEG, -1);
      return;
    } else if (sym->val == "*") {
      visitEach(sExprs->rest,
                [&](std::shared_ptr<SExpr> sExpr) { this->compile(sExpr); });
      getCode().pushCode(OpCode::MULT, -1);
      return;
    } else if (sym->val == "/") {
      visitEach(sExprs->rest,
                [&](std::shared_ptr<SExpr> sExpr) { this->compile(sExpr); });
      getCode().pushCode(OpCode::DIV, -1);
      return;
    } else if (sym->val == "%") {
      visitEach(sExprs->rest,
                [&](std::shared_ptr<SExpr> sExpr) { this->compile(sExpr); });
      getCode().pushCode(OpCode::MOD, -1);
      return;
    }
  }
  compile(sExprs->first);
  const auto argc = visitEach(sExprs->rest, [&](std::shared_ptr<SExpr> sExpr) {
    this->compile(sExpr);
  });
  getCode().pushCode(OpCode::CALL, -1);
  getCode().pushCode(argc, -1);
}

void Compiler::compileSym(std::shared_ptr<SymAtom> sym) {
  auto it = findLocal(sym);
  if (it != locals.rend()) {
    auto idx = std::distance(locals.begin(), it.base()) - 1;
    getCode().pushCode(OpCode::LOAD_FAST, -1);
    getCode().pushCode((uint8_t)idx, -1);
    return;
  }
  getCode().pushCode(OpCode::LOAD_SYM, -1);
  getCode().pushCode(getCode().pushConst(sym), -1);
}

void Compiler::compileDef(std::shared_ptr<SExpr> sExpr) {
  try {
    auto sym = cast<SymAtom>(cast<SExprs>(at(defSymPos, sExpr))->first);
    auto expr = cast<SExprs>(at(defSExprPos, sExpr))->first;
    cast<NilAtom>(at(defNilPos, sExpr));
    compile(expr);
    getCode().pushCode(OpCode::DEF_SYM, -1);
    getCode().pushCode(getCode().pushConst(sym), -1);
  } catch (TypeError &te) {
    handleSyntaxError(defGrammar, sExpr);
  }
}

void Compiler::compileSet(std::shared_ptr<SExpr> sExpr) {
  try {
    auto sym = cast<SymAtom>(cast<SExprs>(at(setSymPos, sExpr))->first);
    auto expr = cast<SExprs>(at(setSExprPos, sExpr))->first;
    cast<NilAtom>(at(setNilPos, sExpr));
    compile(expr);
    auto it = findLocal(sym);
    if (it != locals.rend()) {
      auto idx = std::distance(locals.begin(), it.base()) - 1;
      getCode().pushCode(OpCode::SET_FAST, -1);
      getCode().pushCode(idx, -1);
      return;
    }
    getCode().pushCode(OpCode::SET_SYM, -1);
    getCode().pushCode(getCode().pushConst(sym), -1);
  } catch (TypeError &te) {
    handleSyntaxError(setGrammar, sExpr);
  }
}

void Compiler::compileIf(std::shared_ptr<SExpr> sExpr) {
  try {
    auto test = cast<SExprs>(at(ifTestPos, sExpr))->first;
    auto conseq = cast<SExprs>(at(ifConseqPos, sExpr))->first;
    auto alt = cast<SExprs>(at(ifAltPos, sExpr))->first;
    cast<NilAtom>(at(ifNilPos, sExpr));
    compile(test);
    auto jifIdx = getCode().pushCode(OpCode::POP_JUMP_IF_FALSE, -1) + 1;
    getCode().pushCode(UINT8_MAX, -1);
    getCode().pushCode(UINT8_MAX, -1);
    compile(conseq);
    auto jIdx = getCode().pushCode(OpCode::JUMP, -1) + 1;
    getCode().pushCode(UINT8_MAX, -1);
    getCode().pushCode(UINT8_MAX, -1);
    getCode().patchJump(jifIdx);
    compile(alt);
    getCode().patchJump(jIdx);
  } catch (TypeError &te) {
    handleSyntaxError(ifGrammar, sExpr);
  }
}

void Compiler::compileLambda(std::shared_ptr<SExpr> sExpr) {
  try {
    auto argNames = cast<SExprs>(at(lambdaArgPos, sExpr))->first;
    auto body = cast<SExprs>(at(lambdaBodyPos, sExpr))->first;
    cast<NilAtom>(at(lambdaNilPos, sExpr));
    Compiler compiler(argNames, body, scopeDepth + 1);
    auto function = compiler.compile();
    getCode().pushCode(OpCode::LOAD_CONST, -1);
    getCode().pushCode(getCode().pushConst(function), -1);
  } catch (TypeError &te) {
    handleSyntaxError(lambdaGrammar, sExpr);
  }
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
  for (auto i = 0; i < n; ++i) {
    it = cast<SExprs>(it)->rest;
  }
  return it;
}

void Compiler::beginScope() { scopeDepth += 1; }

void Compiler::endScope() { setScope(scopeDepth - 1); }

void Compiler::setScope(unsigned int scope) {
  scopeDepth = scope;
  while (locals.size() && locals.back().depth > scopeDepth) {
    locals.pop_back();
    getCode().pushCode(OpCode::POP_TOP, -1);
  }
}

std::vector<Compiler::Local>::reverse_iterator
Compiler::findLocal(std::shared_ptr<SymAtom> sym) {
  return std::find_if(locals.rbegin(), locals.rend(),
                      [&sym](Local local) { return *local.symbol == *sym; });
}

void Compiler::handleSyntaxError(std::string expected,
                                 std::shared_ptr<SExpr> actual) {
  std::stringstream ss;
  ss << "Expected \"" << expected << "\", but got \"" << *actual << "\".";
  std::cout << ss.str();
}

Code &Compiler::getCode() { return function->getCode(); }

Compiler::Compiler(std::shared_ptr<SExpr> root)
    : function(std::make_shared<FunctionAtom>(0)),
      argNames(std::make_shared<NilAtom>()), body(root), scopeDepth(0) {}

Compiler::Compiler(std::shared_ptr<SExpr> argNames, std::shared_ptr<SExpr> body,
                   unsigned int scopeDepth)
    : function(nullptr), argNames(argNames), body(body),
      scopeDepth(scopeDepth) {
  locals.push_back({std::make_unique<SymAtom>(""), 0});
  unsigned int arity = 0;
  visitEach(argNames, [&](std::shared_ptr<SExpr> sExpr) {
    arity += 1;
    auto sym = cast<SymAtom>(sExpr);
    locals.push_back({sym, scopeDepth});
  });
  function = std::make_unique<FunctionAtom>(arity);
}
