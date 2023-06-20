#include "../common/OpCode.hpp"
#include "../error/SyntaxError.hpp"
#include "../error/TypeError.hpp"
#include "../sexpr/cast.cpp"
#include "Compiler.hpp"
#include "grammar.hpp"

using namespace sexpr;
using namespace compile;
using namespace error;

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
