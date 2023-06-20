#include "../sexpr/cast.cpp"
#include "Compiler.hpp"
#include "grammar.hpp"

using namespace sexpr;
using namespace compile;
using namespace runtime;
using namespace error;

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
