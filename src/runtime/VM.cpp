#include "VM.hpp"
#include "../error/RuntimeError.hpp"
#include "../sexpr/NatFn.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/cast.cpp"
#include "CppFnImpls.hpp"
#include "StackPtr.hpp"
#include <algorithm>
#include <exception>
#include <iomanip>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

using namespace runtime;
using namespace sexpr;

const SExpr *VM::eval(const Fn *main, bool withGC) {
  try {
    stack.push_back(alloc<Closure>(main));

    enableGC = withGC;

    return exec(main);
  } catch (std::exception &e) {
    std::stringstream ss;
    ss << "Runtime error: " << e.what();
    const auto re = error::RuntimeError(ss.str(), globals, stack, callFrames);
    reset();
    throw re;
  }
  return nullptr;
}

const SExpr *VM::exec(const Fn *main) {
#define CUR_CALL_FRAME() (callFrames.back())
#define CUR_CLOSURE() (CUR_CALL_FRAME().closure)
#define CUR_FN() (CUR_CLOSURE()->fnAtom)
#define CUR_CODE() (CUR_FN()->code)
#define BASE_PTR() (CUR_CALL_FRAME().bp)
#define INST_PTR() (CUR_CALL_FRAME().ip)

#define READ_BYTE() (CUR_CODE().byteCodes[INST_PTR()++])
#define READ_SHORT()                                                           \
  (INST_PTR() += 2, (uint16_t)((CUR_CODE().byteCodes[INST_PTR() - 2] << 8 |    \
                                CUR_CODE().byteCodes[INST_PTR() - 1])))
#define READ_CONST() (CUR_CODE().consts[READ_BYTE()])

#define DISPATCH() goto *dispatchTable[READ_BYTE()]

  static void *dispatchTable[] = {
      &&MAKE_CLOSURE, &&CALL,       &&RETURN,    &&POP_TOP, &&CLOSE_UPVALUE,
      &&LOAD_CONST,   &&LOAD_SYM,   &&DEF_SYM,   &&SET_SYM, &&LOAD_UPVALUE,
      &&SET_UPVALUE,  &&LOAD_STACK, &&SET_STACK, &&JUMP,    &&POP_JUMP_IF_FALSE,
      &&MAKE_LIST,    &&MAKE_NIL};

  call(0);

  DISPATCH();

MAKE_CLOSURE : {
  const auto fnAtom = cast<Fn>(READ_CONST());
  std::vector<std::shared_ptr<Upvalue>> upvalues;
  for (unsigned int i{0}; i < fnAtom->numUpvals; ++i) {
    auto isLocal = READ_BYTE();
    auto idx = READ_BYTE();
    if (isLocal == 1) {
      upvalues.push_back(captureUpvalue(BASE_PTR() + idx));
    } else {
      upvalues.push_back(CUR_CLOSURE()->upvalues[idx]);
    }
  }
  stack.push_back(alloc<Closure>(fnAtom, upvalues));
}
  DISPATCH();
CALL : {
  call(READ_BYTE());
  DISPATCH();
}
RETURN : {
  if (callFrames.size() == 1) {
    const auto res = stack.back();
    reset();
    return res;
  }
  callFrames.pop_back();
  DISPATCH();
}
POP_TOP : {
  stack.pop_back();
  DISPATCH();
}
CLOSE_UPVALUE : {
  auto it = openUpvals.find(stack.size() - 1);
  if (it != openUpvals.end()) {
    it->second->close();
    openUpvals.erase(it);
  }
  stack.pop_back();
}
  DISPATCH();
LOAD_CONST : {
  stack.push_back(READ_CONST());
  DISPATCH();
}
LOAD_SYM : {
  stack.push_back(globals.find(cast<Sym>(READ_CONST())));
  DISPATCH();
}
DEF_SYM : {
  globals.def(cast<Sym>(READ_CONST()), stack.back());
  DISPATCH();
}
SET_SYM : {
  globals.set(cast<Sym>(READ_CONST()), stack.back());
  DISPATCH();
}
LOAD_UPVALUE : {
  stack.push_back(CUR_CLOSURE()->upvalues[READ_BYTE()]->get());
  DISPATCH();
}
SET_UPVALUE : {
  CUR_CLOSURE()->upvalues[READ_BYTE()]->set(stack.back());
  DISPATCH();
}
LOAD_STACK : {
  stack.push_back(stack[BASE_PTR() + READ_BYTE()]);
  DISPATCH();
}
SET_STACK : {
  stack[BASE_PTR() + READ_BYTE()] = stack.back();
  DISPATCH();
}
JUMP : {
  INST_PTR() += READ_SHORT();
  DISPATCH();
}
POP_JUMP_IF_FALSE : {
  auto offset = READ_SHORT();
  if (!Bool::toBool(stack.back())) {
    INST_PTR() += offset;
  }
  stack.pop_back();
  DISPATCH();
}
MAKE_LIST : {
  const auto gcSetting = enableGC;
  enableGC = false;

  const auto n = stack.size() - BASE_PTR() - 1;

  const auto list = makeList(n);
  stack.erase(stack.end() - n, stack.end());
  stack.push_back(list);

  enableGC = gcSetting;
  DISPATCH();
}
MAKE_NIL : {
  stack.push_back(alloc<Nil>());
  DISPATCH();
}

#undef CUR_CALL_FRAME
#undef CUR_CLOSURE
#undef CUR_FN
#undef CUR_CODE
#undef BASE_PTR
#undef INST_PTR

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONST

#undef DISPATCH
}

void VM::call(const uint8_t argc) {
  const auto callee = peak(argc);
  if (isa<Closure>(callee)) {
    const auto closure = cast<Closure>(callee);
    closure->assertArity(argc);
    callFrames.push_back({closure, 0, stack.size() - argc - 1});
    return;
  }
  const auto res = cast<NatFn>(callee)->invoke(stack.end() - argc, argc, *this);
  stack.erase(stack.end() - argc - 1, stack.end());
  stack.push_back(res);
  return;
}

std::shared_ptr<Upvalue> VM::captureUpvalue(StackPtr pos) {
  auto it = openUpvals.find(pos);
  if (it != openUpvals.end()) {
    return it->second;
  }
  openUpvals.insert({pos, std::make_shared<Upvalue>(pos, stack)});
  return openUpvals[pos];
}

const SExpr *VM::peak(StackPtr distance) { return stack.rbegin()[distance]; }

const SExpr *VM::makeList(StackPtr n) {
  if (n == 0) {
    return alloc<Nil>();
  }
  return alloc<SExprs>(*(stack.end() - n), makeList(n - 1));
}

const SExpr *VM::eval(const Fn *main) {
  const auto res = eval(main, false);
  return res;
}

const SExpr *VM::evalWithGC(const Fn *main) {
  const auto res = eval(main, true);
  return res;
}

void VM::reset() {
  stack.clear();
  callFrames.clear();
  enableGC = false;
}

void VM::gc() {
  black.clear();

  markGlobals();
  markStack();
  markCallFrames();
  markOpenUpvalues();

  while (grey.size() > 0) {
    const auto sexpr = grey.front();
    black.emplace(sexpr);
    grey.pop_front();
    trace(sexpr);
  }
  std::erase_if(
      heap, [&](const auto &unique) { return !black.contains(unique.get()); });
}
void VM::mark(const SExpr *sexpr) {
  if (!black.contains(sexpr)) {
    grey.push_back(sexpr);
  }
}
void VM::trace(const SExpr *sexpr) {
  if (const auto sexprs = dynCast<SExprs>(sexpr)) {
    mark(sexprs->first);
    mark(sexprs->rest);
    return;
  }
  if (const auto fnAtom = dynCast<Fn>(sexpr)) {
    std::for_each(fnAtom->code.consts.begin(), fnAtom->code.consts.end(),
                  [&](const auto &sexpr) { mark(sexpr); });
    return;
  }
  if (const auto closureAtom = dynCast<Closure>(sexpr)) {
    mark(closureAtom->fnAtom);
    std::for_each(closureAtom->upvalues.begin(), closureAtom->upvalues.end(),
                  [&](const auto &upvalue) { mark(upvalue->get()); });
    return;
  }
}
void VM::markGlobals() {
  for (const auto &[sym, sexpr] : globals.getSymTable()) {
    grey.push_back(sym);
    grey.push_back(sexpr);
  }
}
void VM::markStack() {
  for (const auto &sexpr : stack) {
    grey.push_back(sexpr);
  }
}
void VM::markCallFrames() {
  for (const auto &callFrame : callFrames) {
    grey.push_back(callFrame.closure);
  }
}
void VM::markOpenUpvalues() {
  for (const auto &[_, openUpvalue] : openUpvals) {
    grey.push_back(openUpvalue->get());
  }
}

VM::VM() : enableGC(false), gcHeapSize(LISP_GC_INIT_HEAP_SIZE) {
  for (double i{LISP_INT_CACHE_MIN}; i <= LISP_INT_CACHE_MAX; i++) {
    intCache.push_back(std::make_unique<Num>(i));
  }

#define BIND_NATIVE_FN(sym, func, argc, isVariadic)                            \
  do {                                                                         \
    globals.def(alloc<Sym>(sym), alloc<NatFn>(&func, argc, isVariadic));       \
  } while (false)

  BIND_NATIVE_FN("sym?", lispIsSym, 1, false);
  BIND_NATIVE_FN("gensym", lispGenSym, 0, false);

  BIND_NATIVE_FN("num?", lispIsNum, 1, false);
  BIND_NATIVE_FN("=", lispNumEq, 1, true);
  BIND_NATIVE_FN(">", lispGt, 1, true);
  BIND_NATIVE_FN(">=", lispGteq, 1, true);
  BIND_NATIVE_FN("<", lispLt, 1, true);
  BIND_NATIVE_FN("<=", lispLteq, 1, true);
  BIND_NATIVE_FN("+", lispAdd, 1, true);
  BIND_NATIVE_FN("*", lispMult, 1, true);
  BIND_NATIVE_FN("-", lispSub, 1, true);
  BIND_NATIVE_FN("/", lispDiv, 1, true);
  BIND_NATIVE_FN("abs", lispAbs, 1, false);
  BIND_NATIVE_FN("%", lispMod, 2, false);

  BIND_NATIVE_FN("str?", lispIsStr, 1, false);
  BIND_NATIVE_FN("str-len", lispStrLen, 1, false);
  BIND_NATIVE_FN("str-sub", lispStrSub, 3, false);
  BIND_NATIVE_FN("str-con", lispStrCon, 1, true);
  BIND_NATIVE_FN("->str", lispToStr, 1, false);

  BIND_NATIVE_FN("null?", lispIsNull, 1, false);
  BIND_NATIVE_FN("cons?", lispIsCons, 1, false);
  BIND_NATIVE_FN("cons", lispCons, 2, false);
  BIND_NATIVE_FN("car", lispCar, 1, false);
  BIND_NATIVE_FN("cdr", lispCdr, 1, false);

  BIND_NATIVE_FN("dis", lispDis, 1, false);
  BIND_NATIVE_FN("display", lispDisplay, 1, false);

  BIND_NATIVE_FN("quit", lispQuit, 0, false);
  BIND_NATIVE_FN("error", lispError, 1, false);

  BIND_NATIVE_FN("eq?", lispEq, 2, false);
  BIND_NATIVE_FN("eqv?", lispEqv, 2, false);

  BIND_NATIVE_FN("proc?", lispIsProc, 1, false);

#undef BIND_NATIVE_FN
}

void VM::defMacro(const Sym *sym) { globals.defMacro(sym); }

bool VM::isMacro(const Sym *sym) { return globals.isMacro(sym); }
