#include "CodeGenerator.hpp"
#include "../code/OpCode.hpp"
#include "../error/SyntaxError.hpp"
#include "../runtime/VM.hpp"
#include "../sexpr/Casting.hpp"
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

CodeGenerator::CodeGenerator(
    runtime::VM &vm,
    CodeGenerator &enclosing,
    ParsedSrc &parsedSrc,
    const sexpr::SExpr *param,
    const sexpr::SExprs *body
)
    : vm(vm),
      enclosing(enclosing),
      parsedSrc(parsedSrc),
      curSrcLoc({parsedSrc.srcMap[param].row, parsedSrc.srcMap[param].col}),
      param(param),
      body(body),
      arity(countArity()),
      variadic(isVariadic()),
      stackOffset(1),
      proto(generate()) {}

void CodeGenerator::updateCurSrcLoc(const sexpr::SExprs *sExpr) {
  curSrcLoc = parsedSrc.srcMap[sExpr];
}

std::optional<const std::size_t> CodeGenerator::resolveLocal(const Sym *sym) {
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
CodeGenerator::resolveUpvalue(CodeGenerator &caller, const Sym *sym) {
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

std::size_t CodeGenerator::addUpvalue(int idx, bool isLocal) {
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

bool CodeGenerator::isVariadic() { return isa<Sym>(last(param)); }

uint8_t CodeGenerator::countArity() {
  if (isa<Nil>(param) || isa<Sym>(param)) {
    return 0;
  }
  return visitEach(cast<SExprs>(param), [](const auto) {});
}

code::InstrPtr CodeGenerator::emitConst(const sexpr::SExpr *sExpr) {
  return code.pushConst(sExpr);
}

void CodeGenerator::patchJump(const code::InstrPtr idx) { code.patchJump(idx); }

const SExpr *CodeGenerator::last(const SExpr *sExpr) {
  if (isa<Atom>(sExpr)) {
    return sExpr;
  }
  const auto sExprs = cast<SExprs>(sExpr);
  updateCurSrcLoc(sExprs);
  return last(sExprs->rest);
}

unsigned int CodeGenerator::visitEach(const SExpr *sExpr, Visitor visitor) {
  if (isa<Atom>(sExpr)) {
    return 0;
  }
  const auto sExprs = cast<SExprs>(sExpr);
  updateCurSrcLoc(sExprs);
  visitor(sExprs->first);
  return 1 + visitEach(sExprs->rest, visitor);
}

void CodeGenerator::traverse(const SExpr *sExpr, Visitor visitor) {
  if (isa<SExprs>(sExpr)) {
    const auto sexprs = cast<SExprs>(sExpr);
    traverse(sexprs->first, visitor);
    traverse(sexprs->rest, visitor);
  }
  visitor(sExpr);
}

const Prototype *CodeGenerator::generate() {
  if (const auto sExprs = dynCast<SExprs>(param)) {
    visitEach(sExprs.value(), [this](const auto sExpr) {
      const auto sym = cast<Sym>(sExpr);
      locals.push_back({sym, stackOffset, false});
      stackOffset += 1;
    });
  }

  const auto lastParam = last(param);

  if (const auto sym = dynCast<Sym>(lastParam)) {
    locals.push_back({sym.value(), stackOffset, false});
    stackOffset += 1;
  }

  if (variadic) {
    emitCode(OpCode::MAKE_LIST, arity + 1);
  }

  emitStmts(body);
  emitRet();

  return vm.heap.alloc<Prototype>(upValues.size(), arity, variadic, code);
}

void CodeGenerator::emitStmts(const SExpr *sExpr) {
  emitCode(OpCode::MAKE_NIL);
  visitEach(sExpr, [this](const auto sExpr) {
    stackOffset += 1;
    emitStmt(sExpr);
  });
}

void CodeGenerator::emitExprs(const SExpr *sExpr) {
  emitCode(OpCode::MAKE_NIL);
  visitEach(sExpr, [this](const auto sExpr) {
    emitCode(OpCode::POP_TOP);
    emitExpr(sExpr);
  });
}

void CodeGenerator::emitStmt(const SExpr *sExpr) {
  if (matchForm(
          sExpr,
          {
              {&DEFINE_SYM, [this](const auto &matched) { emitDef(matched); }},
              {&DEFMACRO_SYM,
               [this](const auto &matched) { execDefMacro(matched); }},
              {&BEGIN_SYM,
               [this](const auto &matched) { emitStmts(matched.get()); }},
          },
          [this, &sExpr](const auto &sym, const auto) {
            if (vm.env.isMacro(sym.get())) {
              emitStmt(execMacro(sExpr));
              return;
            }
            emitExpr(sExpr);
          }
      )) {
    return;
  }
  emitExpr(sExpr);
}

void CodeGenerator::emitExpr(const SExpr *sExpr) {
  if (matchForm(
          sExpr,
          {{&DEFINE_SYM, [this](const auto &) { handleInvalidDef(); }},
           {&DEFMACRO_SYM, [this](const auto &) { handleInvalidDef(); }},
           {&QUOTE_SYM, [this](const auto &matched) { emitQuote(matched); }},
           {&SET_SYM, [this](const auto &matched) { emitSet(matched); }},
           {&IF_SYM, [this](const auto &matched) { emitIf(matched); }},
           {&LAMBDA_SYM, [this](const auto &matched) { emitLambda(matched); }},
           {&BEGIN_SYM,
            [this](const auto &matched) { emitExprs(matched.get()); }}},
          [this, &sExpr](const auto sym, const auto) {
            if (vm.env.isMacro(sym.get())) {
              emitExpr(execMacro(sExpr));
              return;
            }
            emitCall(cast<SExprs>(sExpr));
          }
      )) {
    return;
  };
  if (const auto atom = dynCast<Atom>(sExpr)) {
    emitAtom(atom.value());
    return;
  }
  emitCall(cast<SExprs>(sExpr));
}

void CodeGenerator::emitAtom(const Atom *atom) {
  if (isa<Nil>(atom)) {
    throw error::SyntaxError(
        "Expected a non-empty list.",
        parsedSrc.source[curSrcLoc.row - 1],
        curSrcLoc.row,
        curSrcLoc.col
    );
  }
  if (const auto sym = dynCast<Sym>(atom)) {
    emitSym(sym.value());
    return;
  }
  emitCode(OpCode::LOAD_CONST, emitConst(atom));
}

void CodeGenerator::emitCall(const SExprs *sExprs) {
  emitExpr(sExprs->first);
  const auto argc =
      visitEach(sExprs->rest, [this](const auto &sExpr) { emitExpr(sExpr); });

  try {
    cast<Nil>(last(sExprs));
  } catch (error::TypeError &te) {
    handleTypeError(callGrammar, te.expected, te.actual);
  }
  emitCode(OpCode::CALL, argc);
}

void CodeGenerator::emitLambda(const MatchedSExpr<sexpr::SExpr> matched) {
  try {
    const auto [lambdaParam, lambdaBody] = unpackPartial<SExpr>(matched.get());

    if (isa<SExprs>(lambdaParam.get())) {
      visitEach(lambdaParam.get(), [](const auto &argName) {
        assertType<Sym>(argName);
      });
      assertType<Sym, Nil>(last(lambdaParam.get()));
    }

    CodeGenerator codeGenerator(
        vm, *this, parsedSrc, lambdaParam.get(), cast<SExprs>(lambdaBody.get())
    );
    const auto function = codeGenerator.getGenerated();

    emitCode(OpCode::MAKE_CLOSURE, emitConst(function));
    for (const auto &upValue : codeGenerator.upValues) {
      emitCode(upValue.isLocal ? 1 : 0, upValue.idx);
    }
  } catch (error::TypeError &te) {
    handleTypeError(lambdaGrammar, te.expected, te.actual);
  }
}

void CodeGenerator::emitSym(const sexpr::Sym *sym) {
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

void CodeGenerator::emitQuote(const MatchedSExpr<sexpr::SExpr> matched) {
  try {
    const auto [expr] = unpack<SExpr>(matched.get());

    emitCode(OpCode::LOAD_CONST, emitConst(expr.get()));
  } catch (error::TypeError &te) {
    handleTypeError(quoteGrammar, te.expected, te.actual);
  }
}

void CodeGenerator::emitDef(const MatchedSExpr<sexpr::SExpr> matched) {
  try {
    const auto [sym, expr] = unpack<Sym, SExpr>(matched.get());

    emitExpr(expr.get());
    if (enclosing.has_value()) {
      locals.push_back({sym.get(), stackOffset, false});
    } else {
      emitCode(OpCode::DEF_SYM, emitConst(sym.get()));
    }
  } catch (error::TypeError &te) {
    handleTypeError(defGrammar, te.expected, te.actual);
  }
}

void CodeGenerator::execDefMacro(const MatchedSExpr<sexpr::SExpr> matched) {
  if (enclosing.has_value()) {
    const auto [row, col] = curSrcLoc;
    throw error::SyntaxError(
        "Invalid syntax for define-macro: must define macros in top level",
        parsedSrc.source[row - 1],
        row,
        col
    );
  }
  try {
    const auto [macroSym, macroArgNames, macroBody] =
        unpackPartial<Sym, SExpr>(matched.get());

    CodeGenerator codeGenerator(
        vm, *this, parsedSrc, macroArgNames.get(), cast<SExprs>(macroBody.get())
    );
    const auto function = codeGenerator.getGenerated();

    Code def;

    def.pushCode(OpCode::MAKE_CLOSURE, curSrcLoc.row);
    def.pushCode(def.pushConst(function));

    for (const auto &upValue : codeGenerator.upValues) {
      def.pushCode(upValue.isLocal ? 1 : 0);
      def.pushCode(upValue.idx);
    }

    def.pushCode(OpCode::DEF_MACRO, curSrcLoc.row);
    def.pushCode(def.pushConst(macroSym.get()));
    def.pushCode(OpCode::RETURN, curSrcLoc.row);

    vm.load(vm.heap.alloc<Prototype>(0, 0, false, def));
    vm.eval();

    emitCode(OpCode::MAKE_NIL);
    assertType<Nil>(last(macroBody.get()));
  } catch (error::TypeError &te) {
    handleTypeError(defMacroGrammar, te.expected, te.actual);
  }
}

void CodeGenerator::emitSet(const MatchedSExpr<sexpr::SExpr> matched) {
  try {
    const auto [sym, expr] = unpack<Sym, SExpr>(matched.get());

    emitExpr(expr.get());
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

void CodeGenerator::emitIf(const MatchedSExpr<sexpr::SExpr> matched) {
  try {
    const auto [test, conseq, alt] = unpack<SExpr, SExpr, SExpr>(matched.get());

    emitExpr(test.get());
    const auto jifIdx =
        emitCode(OpCode::POP_JUMP_IF_FALSE, UINT8_MAX, UINT8_MAX) + 1;

    emitExpr(conseq.get());
    const auto jIdx = emitCode(OpCode::JUMP, UINT8_MAX, UINT8_MAX) + 1;
    patchJump(jifIdx);

    emitExpr(alt.get());
    patchJump(jIdx);
  } catch (error::TypeError &te) {
    handleTypeError(ifGrammar, te.expected, te.actual);
  }
}

void CodeGenerator::emitRet() {
  try {
    cast<Nil>(last(body));
  } catch (error::TypeError &te) {
    handleTypeError(lambdaGrammar, te.expected, te.actual);
  }

  for (const auto &local : locals) {
    if (local.isCaptured) {
      emitCode(OpCode::CLOSE_UPVALUE, local.stackOffset);
    }
  }

  emitCode(OpCode::RETURN);
}

const SExpr *CodeGenerator::execMacro(const SExpr *sExpr) {
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

  vm.load(vm.heap.alloc<Prototype>(0, 0, false, fExpr));
  const auto res = vm.eval();

  traverse(res, [this](const auto &sExpr) {
    parsedSrc.srcMap.insert({sExpr, curSrcLoc});
  });

  return res;
}

void CodeGenerator::handleInvalidDef() {
  const auto [row, col] = curSrcLoc;
  throw error::SyntaxError(
      "Invalid syntax for define: cannot use define as an "
      "expression",
      parsedSrc.source[row - 1],
      row,
      col
  );
}

void CodeGenerator::handleTypeError(
    const std::string grammar, const std::string expected, const SExpr *actual
) {
  std::stringstream ss;
  ss << "Invalid syntax for " << grammar << "." << std::endl
     << "Expected " << expected << ", but got " << actual << ".";
  const auto [row, col] = curSrcLoc;
  throw SyntaxError(ss.str(), parsedSrc.source[row - 1], row, col);
}

CodeGenerator::CodeGenerator(runtime::VM &vm, ParsedSrc &parsedSrc)
    : vm(vm),
      gcGuard(vm.heap.pauseGC()),
      parsedSrc(parsedSrc),
      curSrcLoc({1, 0}),
      param(vm.heap.alloc<Nil>()),
      body(parsedSrc.root),
      arity(0),
      variadic(false),
      stackOffset(1),
      proto(generate()) {}

const Prototype *CodeGenerator::getGenerated() const { return proto; }
