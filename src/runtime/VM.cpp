#include "VM.hpp"
#include "../error/RuntimeError.hpp"
#include "../sexpr/Cast.cpp"
#include "../sexpr/NatFn.hpp"
#include "../sexpr/SExprs.hpp"
#include "CPPFnImpls.hpp"
#include "CallFrame.hpp"
#include "FreeStore.hpp"
#include "GCGuard.hpp"
#include "StackIter.hpp"
#include "StackPtr.hpp"
#include <algorithm>
#include <exception>
#include <iomanip>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

using namespace runtime;
using namespace sexpr;
using namespace code;

const SExpr &VM::eval(const Fn &main, bool withGC) {
  stack.push_back(freeStore.alloc<Closure>(main));
  try {
    if (withGC) {
      auto gcGuard = freeStore.startGC();
      return exec();
    }
    return exec();
  } catch (std::exception &e) {
    std::stringstream ss;
    ss << "Runtime error: " << e.what();
    const auto re = error::RuntimeError(ss.str(), globals, stack, callFrames);
    reset();
    throw re;
  }
  return freeStore.alloc<Nil>();
}

inline CallFrame &VM::callFrame() { return callFrames.back(); }
inline const Closure &VM::closure() { return callFrame().closure; }
inline const Fn &VM::fn() { return closure().fn; }
inline const Code &VM::code() { return fn().code; }

inline code::InstrPtr &VM::instPtr() { return callFrame().ip; }
inline runtime::StackPtr &VM::basePtr() { return callFrame().bp; }

inline const sexpr::SExpr &VM::readConst() {
  return code().consts[readByte()].get();
}
inline uint8_t VM::readByte() { return code().byteCodes[instPtr()++]; }
inline uint16_t VM::readShort() {
  instPtr() += 2;
  return (uint16_t)((code().byteCodes[instPtr() - 2] << 8 |
                     code().byteCodes[instPtr() - 1]));
}

void VM::call(const uint8_t argc) {
  const auto &callee = peak(argc);
  if (isa<Closure>(callee)) {
    const auto &closure = cast<Closure>(callee);
    closure.assertArity(argc);
    callFrames.push_back({closure, 0, stack.size() - argc - 1});
    return;
  }
  if (isa<NatFn>(callee)) {
    const auto &natFn = cast<NatFn>(callee);
    const auto &res = natFn.invoke(stack.end() - argc, argc, *this);
    if (!natFn.abandonsCont) {
      stack.erase(stack.end() - argc - 1, stack.end());
      stack.push_back(res);
    }
    return;
  }
  std::stringstream ss;
  ss << "Expected a closure or native function as callee, but got " << callee
     << ".";
  const auto re = std::invalid_argument(ss.str());
  throw re;
}

std::shared_ptr<Upvalue> VM::captureUpvalue(StackPtr pos) {
  auto it = openUpvals.find(pos);
  if (it != openUpvals.end()) {
    return it->second;
  }
  openUpvals.insert({pos, std::make_shared<Upvalue>(pos, stack)});
  return openUpvals[pos];
}

const SExpr &VM::peak(StackPtr distance) {
  return stack.rbegin()[distance].get();
}

const SExpr &VM::makeList(StackIter start) {
  if (start == stack.cend()) {
    return freeStore.alloc<Nil>();
  }
  return freeStore.alloc<SExprs>(*start, makeList(start + 1));
}

unsigned int VM::unpackList(const SExpr &sexpr) {
  if (isa<Nil>(sexpr)) {
    return 0;
  }
  const auto &sexprs = cast<SExprs>(sexpr);
  stack.push_back(sexprs.first);
  return 1 + unpackList(sexprs.rest);
}

void VM::reset() {
  stack.clear();
  callFrames.clear();
}

const SExpr &VM::exec() {

#define DISPATCH() goto *dispatchTable[readByte()]

  static void *dispatchTable[] = {
      &&MAKE_CLOSURE,      &&CALL,        &&RETURN,     &&POP_TOP,   &&POP,
      &&CLOSE_UPVALUE,     &&LOAD_CONST,  &&LOAD_SYM,   &&DEF_SYM,   &&SET_SYM,
      &&LOAD_UPVALUE,      &&SET_UPVALUE, &&LOAD_STACK, &&SET_STACK, &&JUMP,
      &&POP_JUMP_IF_FALSE, &&MAKE_LIST,   &&MAKE_NIL};

  call(0);

  DISPATCH();

MAKE_CLOSURE : {
  const auto &fnAtom = cast<Fn>(readConst());
  std::vector<std::shared_ptr<Upvalue>> upvalues;
  for (unsigned int i{0}; i < fnAtom.numUpvals; ++i) {
    auto isLocal = readByte();
    auto idx = readByte();
    if (isLocal == 1) {
      upvalues.push_back(captureUpvalue(basePtr() + idx));
    } else {
      upvalues.push_back(closure().upvalues[idx]);
    }
  }
  stack.push_back(freeStore.alloc<Closure>(fnAtom, upvalues));
}
  DISPATCH();
CALL : {
  call(readByte());
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
POP : {
  stack.erase(stack.end() - readByte(), stack.end());
  DISPATCH();
}
CLOSE_UPVALUE : {
  auto it = openUpvals.find(basePtr() + readByte());
  if (it != openUpvals.end()) {
    it->second->close();
    openUpvals.erase(it);
  }
  DISPATCH();
}
LOAD_CONST : {
  stack.push_back(readConst());
  DISPATCH();
}
LOAD_SYM : {
  const auto &sym = cast<Sym>(readConst());
  stack.push_back(globals.find(sym));
  if (isa<Undefined>(stack.back())) [[unlikely]] {
    std::stringstream ss;
    ss << "Access of an undefined global: " << sym << ".";
    throw std::invalid_argument(ss.str());
  }
  DISPATCH();
}
DEF_SYM : {
  globals.def(cast<Sym>(readConst()), stack.back());
  stack.back() = freeStore.alloc<Nil>();
  DISPATCH();
}
SET_SYM : {
  globals.set(cast<Sym>(readConst()), stack.back());
  stack.back() = freeStore.alloc<Nil>();
  DISPATCH();
}
LOAD_UPVALUE : {
  stack.push_back(closure().upvalues[readByte()]->get());
  if (isa<Undefined>(stack.back())) [[unlikely]] {
    throw std::invalid_argument("Access of an undefined upvalue.");
  }
  DISPATCH();
}
SET_UPVALUE : {
  closure().upvalues[readByte()]->set(stack.back());
  stack.back() = freeStore.alloc<Nil>();
  DISPATCH();
}
LOAD_STACK : {
  stack.push_back(stack[basePtr() + readByte()]);
  if (isa<Undefined>(stack.back())) [[unlikely]] {
    throw std::invalid_argument("Access of an undefined local.");
  }
  DISPATCH();
}
SET_STACK : {
  stack[basePtr() + readByte()] = stack.back();
  stack.back() = freeStore.alloc<Nil>();
  DISPATCH();
}
JUMP : {
  instPtr() += readShort();
  DISPATCH();
}
POP_JUMP_IF_FALSE : {
  auto offset = readShort();
  if (!Bool::toBool(stack.back())) {
    instPtr() += offset;
  }
  stack.pop_back();
  DISPATCH();
}
MAKE_LIST : {
  {
    auto gcGuard = freeStore.pauseGC();

    const auto start = stack.begin() + basePtr() + readByte();
    const auto &list = makeList(start);
    stack.erase(start, stack.end());
    stack.push_back(list);
  }

  DISPATCH();
}
MAKE_NIL : {
  stack.push_back(freeStore.alloc<Nil>());
  DISPATCH();
}

#undef DISPATCH
}

VM::VM() : freeStore(globals, stack, callFrames, openUpvals) {
  globals.def(freeStore.alloc<Sym>("sym?"),
              freeStore.alloc<NatFn>(lispIsSym, 1, false));
  globals.def(freeStore.alloc<Sym>("gensym"),
              freeStore.alloc<NatFn>(lispGenSym, 0, false));

  globals.def(freeStore.alloc<Sym>("num?"),
              freeStore.alloc<NatFn>(lispIsNum, 1, false));
  globals.def(freeStore.alloc<Sym>("="),
              freeStore.alloc<NatFn>(lispNumEq, 1, true));
  globals.def(freeStore.alloc<Sym>(">"),
              freeStore.alloc<NatFn>(lispGt, 1, true));
  globals.def(freeStore.alloc<Sym>(">="),
              freeStore.alloc<NatFn>(lispGteq, 1, true));
  globals.def(freeStore.alloc<Sym>("<"),
              freeStore.alloc<NatFn>(lispLt, 1, true));
  globals.def(freeStore.alloc<Sym>("<="),
              freeStore.alloc<NatFn>(lispLteq, 1, true));
  globals.def(freeStore.alloc<Sym>("+"),
              freeStore.alloc<NatFn>(lispAdd, 1, true));
  globals.def(freeStore.alloc<Sym>("*"),
              freeStore.alloc<NatFn>(lispMult, 1, true));
  globals.def(freeStore.alloc<Sym>("-"),
              freeStore.alloc<NatFn>(lispSub, 1, true));
  globals.def(freeStore.alloc<Sym>("/"),
              freeStore.alloc<NatFn>(lispDiv, 1, true));
  globals.def(freeStore.alloc<Sym>("abs"),
              freeStore.alloc<NatFn>(lispAbs, 1, false));
  globals.def(freeStore.alloc<Sym>("%"),
              freeStore.alloc<NatFn>(lispMod, 2, false));

  globals.def(freeStore.alloc<Sym>("string?"),
              freeStore.alloc<NatFn>(lispIsStr, 1, false));
  globals.def(freeStore.alloc<Sym>("string-length"),
              freeStore.alloc<NatFn>(lispStrLen, 1, false));
  globals.def(freeStore.alloc<Sym>("substring"),
              freeStore.alloc<NatFn>(lispStrSub, 3, false));
  globals.def(freeStore.alloc<Sym>("string-append"),
              freeStore.alloc<NatFn>(lispStrCon, 1, true));
  globals.def(freeStore.alloc<Sym>("->str"),
              freeStore.alloc<NatFn>(lispToStr, 1, false));

  globals.def(freeStore.alloc<Sym>("null?"),
              freeStore.alloc<NatFn>(lispIsNull, 1, false));
  globals.def(freeStore.alloc<Sym>("pair?"),
              freeStore.alloc<NatFn>(lispIsCons, 1, false));
  globals.def(freeStore.alloc<Sym>("cons"),
              freeStore.alloc<NatFn>(lispCons, 2, false));
  globals.def(freeStore.alloc<Sym>("car"),
              freeStore.alloc<NatFn>(lispCar, 1, false));
  globals.def(freeStore.alloc<Sym>("cdr"),
              freeStore.alloc<NatFn>(lispCdr, 1, false));

  globals.def(freeStore.alloc<Sym>("dis"),
              freeStore.alloc<NatFn>(lispDis, 1, false));
  globals.def(freeStore.alloc<Sym>("display"),
              freeStore.alloc<NatFn>(lispDisplay, 1, false));

  globals.def(freeStore.alloc<Sym>("quit"),
              freeStore.alloc<NatFn>(lispQuit, 0, false));
  globals.def(freeStore.alloc<Sym>("error"),
              freeStore.alloc<NatFn>(lispError, 1, false));

  globals.def(freeStore.alloc<Sym>("eq?"),
              freeStore.alloc<NatFn>(lispEq, 2, false));
  globals.def(freeStore.alloc<Sym>("eqv?"),
              freeStore.alloc<NatFn>(lispEqv, 2, false));
  globals.def(freeStore.alloc<Sym>("equal?"),
              freeStore.alloc<NatFn>(lispEqual, 2, false));

  globals.def(freeStore.alloc<Sym>("proc?"),
              freeStore.alloc<NatFn>(lispIsProc, 1, false));

  globals.def(freeStore.alloc<Sym>("apply"),
              freeStore.alloc<NatFn>(lispApply, 2, true, true));
}

void VM::regMacro(const Sym &sym) { globals.regMacro(sym); }

bool VM::isMacro(const Sym &sym) { return globals.isMacro(sym); }

const SExpr &VM::eval(const Fn &main) {
  const auto &res = eval(main, false);
  return res;
}

const SExpr &VM::evalWithGC(const Fn &main) {
  const auto &res = eval(main, true);
  return res;
}
