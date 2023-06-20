#include "VM.hpp"
#include "../sexpr/NatFn.hpp"
#include "VM_eval.cc"
#include "VM_mem.cc"
#include <algorithm>
#include <exception>
#include <iomanip>
#include <memory>
#include <string>
#include <vector>

using namespace runtime;
using namespace sexpr;

VM::VM() : enableGC(false), gcHeapSize(LISP_GC_INIT_HEAP_SIZE) {
  for (double i{LISP_INT_CACHE_MIN}; i <= LISP_INT_CACHE_MAX; i++) {
    intCache.push_back(std::make_unique<Num>(i));
  }

#define BIND_NATIVE_FN(sym, func, argc)                                        \
  do {                                                                         \
    globals.def(alloc<Sym>(sym), alloc<NatFn>(&func, argc));                   \
  } while (false)

  BIND_NATIVE_FN("sym?", lispIsSym, 1);
  BIND_NATIVE_FN("gensym", lispGenSym, 0);

  BIND_NATIVE_FN("num?", lispIsNum, 1);
  BIND_NATIVE_FN("=", lispNumEq, -1);
  BIND_NATIVE_FN(">", lispGt, -1);
  BIND_NATIVE_FN(">=", lispGteq, -1);
  BIND_NATIVE_FN("<", lispLt, -1);
  BIND_NATIVE_FN("<=", lispLteq, -1);
  BIND_NATIVE_FN("+", lispAdd, -1);
  BIND_NATIVE_FN("*", lispMult, -1);
  BIND_NATIVE_FN("-", lispSub, -1);
  BIND_NATIVE_FN("/", lispDiv, -1);
  BIND_NATIVE_FN("abs", lispAbs, 1);
  BIND_NATIVE_FN("%", lispMod, 2);

  BIND_NATIVE_FN("str?", lispIsStr, 1);
  BIND_NATIVE_FN("str-len", lispStrLen, 1);
  BIND_NATIVE_FN("str-sub", lispStrSub, 3);
  BIND_NATIVE_FN("str-con", lispStrCon, -1);
  BIND_NATIVE_FN("->str", lispToStr, 1);

  BIND_NATIVE_FN("null?", lispIsNull, 1);
  BIND_NATIVE_FN("cons?", lispIsCons, 1);
  BIND_NATIVE_FN("cons", lispCons, 2);
  BIND_NATIVE_FN("car", lispCar, 1);
  BIND_NATIVE_FN("cdr", lispCdr, 1);

  BIND_NATIVE_FN("dis", lispDis, 1);
  BIND_NATIVE_FN("display", lispDisplay, 1);

  BIND_NATIVE_FN("quit", lispQuit, 0);
  BIND_NATIVE_FN("error", lispError, 1);

  BIND_NATIVE_FN("eq?", lispEq, 2);
  BIND_NATIVE_FN("eqv?", lispEqv, 2);

  BIND_NATIVE_FN("proc?", lispIsProc, 1);

#undef BIND_NATIVE_FN
}

void VM::defMacro(const Sym *sym) { globals.defMacro(sym); }

bool VM::isMacro(const Sym *sym) { return globals.isMacro(sym); }
