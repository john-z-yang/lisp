#include "VM.hpp"
#include "../error/RuntimeError.hpp"
#include "../fn/CPPFnImpls.hpp"
#include "../sexpr/Cast.cpp"
#include "../sexpr/NatFn.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/String.hpp"
#include "CallFrame.hpp"
#include "FreeStore.hpp"
#include "GCGuard.hpp"
#include "StackIter.hpp"
#include "StackPtr.hpp"
#include <algorithm>
#include <exception>
#include <functional>
#include <iomanip>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

using namespace code;
using namespace fn;
using namespace sexpr;
using namespace runtime;

const SExpr &VM::eval(const Prototype &main, bool withGC) {
  closure = freeStore.alloc<Closure>(main);
  stack.push_back(closure->get());
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

inline const sexpr::SExpr &VM::readConst() {
  return closure->get().fn.code.consts[readByte()].get();
}
inline uint8_t VM::readByte() { return closure->get().fn.code.byteCodes[ip++]; }
inline uint16_t VM::readShort() {
  ip += 2;
  return (uint16_t)((closure->get().fn.code.byteCodes[ip - 2] << 8 |
                     closure->get().fn.code.byteCodes[ip - 1]));
}

void VM::call(const uint8_t argc) {
  const auto &callee = peak(argc);
  if (isa<Closure>(callee)) {
    callFrames.push_back({closure->get(), bp, ip});
    ip = 0;
    bp = stack.size() - argc - 1;
    closure = cast<Closure>(callee);
    closure->get().assertArity(argc);
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
  ip = 0;
  bp = 0;
  closure.reset();
  stack.clear();
  callFrames.clear();
}

const SExpr &VM::exec() {
#define DISPATCH() goto *dispatchTable[readByte()]

  void *dispatchTable[] = {
      &&MAKE_CLOSURE,      &&CALL,        &&RETURN,     &&POP_TOP,   &&POP,
      &&CLOSE_UPVALUE,     &&LOAD_CONST,  &&LOAD_SYM,   &&DEF_SYM,   &&SET_SYM,
      &&LOAD_UPVALUE,      &&SET_UPVALUE, &&LOAD_STACK, &&SET_STACK, &&JUMP,
      &&POP_JUMP_IF_FALSE, &&MAKE_LIST,   &&MAKE_NIL};

  call(0);

  DISPATCH();

MAKE_CLOSURE : {
  const auto &fnAtom = cast<Prototype>(readConst());
  std::vector<std::shared_ptr<Upvalue>> upvalues;
  for (unsigned int i{0}; i < fnAtom.numUpvals; ++i) {
    auto isLocal = readByte();
    auto idx = readByte();
    if (isLocal == 1) {
      upvalues.push_back(captureUpvalue(bp + idx));
    } else {
      upvalues.push_back(closure->get().upvalues[idx]);
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
  ip = callFrames.back().ip;
  bp = callFrames.back().bp;
  closure = callFrames.back().closure;
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
  auto it = openUpvals.find(bp + readByte());
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
  stack.push_back(closure->get().upvalues[readByte()]->get());
  if (isa<Undefined>(stack.back())) [[unlikely]] {
    throw std::invalid_argument("Access of an undefined upvalue.");
  }
  DISPATCH();
}
SET_UPVALUE : {
  closure->get().upvalues[readByte()]->set(stack.back());
  stack.back() = freeStore.alloc<Nil>();
  DISPATCH();
}
LOAD_STACK : {
  stack.push_back(stack[bp + readByte()]);
  if (isa<Undefined>(stack.back())) [[unlikely]] {
    throw std::invalid_argument("Access of an undefined local.");
  }
  DISPATCH();
}
SET_STACK : {
  stack[bp + readByte()] = stack.back();
  stack.back() = freeStore.alloc<Nil>();
  DISPATCH();
}
JUMP : {
  ip += readShort();
  DISPATCH();
}
POP_JUMP_IF_FALSE : {
  auto offset = readShort();
  if (!Bool::toBool(stack.back())) {
    ip += offset;
  }
  stack.pop_back();
  DISPATCH();
}
MAKE_LIST : {
  {
    auto gcGuard = freeStore.pauseGC();

    const auto start = stack.begin() + bp + readByte();
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

VM::VM()
    : ip(0), bp(0), freeStore(globals, closure, stack, callFrames, openUpvals) {
  globals.def(freeStore.alloc<Sym>("symbol?"),
              freeStore.alloc<NatFn>(typePred<Sym>, 1, false));
  globals.def(freeStore.alloc<Sym>("gensym"),
              freeStore.alloc<NatFn>(genSym, 0, false));

  globals.def(freeStore.alloc<Sym>("number?"),
              freeStore.alloc<NatFn>(typePred<Num>, 1, false));
  globals.def(freeStore.alloc<Sym>("="),
              freeStore.alloc<NatFn>(compare<Num, std::equal_to>, 1, true));
  globals.def(freeStore.alloc<Sym>(">"),
              freeStore.alloc<NatFn>(compare<Num, std::greater>, 1, true));
  globals.def(
      freeStore.alloc<Sym>(">="),
      freeStore.alloc<NatFn>(compare<Num, std::greater_equal>, 1, true));
  globals.def(freeStore.alloc<Sym>("<"),
              freeStore.alloc<NatFn>(compare<Num, std::less>, 1, true));
  globals.def(freeStore.alloc<Sym>("<="),
              freeStore.alloc<NatFn>(compare<Num, std::less_equal>, 1, true));
  globals.def(freeStore.alloc<Sym>("+"),
              freeStore.alloc<NatFn>(accum<Num, std::plus, 0>, 1, true));
  globals.def(freeStore.alloc<Sym>("*"),
              freeStore.alloc<NatFn>(accum<Num, std::multiplies, 1>, 1, true));
  globals.def(
      freeStore.alloc<Sym>("-"),
      freeStore.alloc<NatFn>(dimi<Num, std::minus, std::negate>, 1, true));
  globals.def(
      freeStore.alloc<Sym>("/"),
      freeStore.alloc<NatFn>(dimi<Num, std::divides, inverse>, 1, true));
  globals.def(freeStore.alloc<Sym>("abs"),
              freeStore.alloc<NatFn>(numAbs, 1, false));
  globals.def(freeStore.alloc<Sym>("modulo"),
              freeStore.alloc<NatFn>(numMod, 2, false));

  globals.def(freeStore.alloc<Sym>("string?"),
              freeStore.alloc<NatFn>(typePred<String>, 1, false));
  globals.def(freeStore.alloc<Sym>("string-length"),
              freeStore.alloc<NatFn>(strLen, 1, false));
  globals.def(freeStore.alloc<Sym>("substring"),
              freeStore.alloc<NatFn>(substr, 3, false));
  globals.def(freeStore.alloc<Sym>("string-append"),
              freeStore.alloc<NatFn>(strApp, 1, true));
  globals.def(freeStore.alloc<Sym>("->str"),
              freeStore.alloc<NatFn>(toStr, 1, false));
  globals.def(freeStore.alloc<Sym>("string=?"),
              freeStore.alloc<NatFn>(compare<String, std::equal_to>, 1, true));
  globals.def(freeStore.alloc<Sym>("string>?"),
              freeStore.alloc<NatFn>(compare<String, std::greater>, 1, true));
  globals.def(
      freeStore.alloc<Sym>("string>=?"),
      freeStore.alloc<NatFn>(compare<String, std::greater_equal>, 1, true));
  globals.def(freeStore.alloc<Sym>("string<?"),
              freeStore.alloc<NatFn>(compare<String, std::less>, 1, true));
  globals.def(
      freeStore.alloc<Sym>("string<=?"),
      freeStore.alloc<NatFn>(compare<String, std::less_equal>, 1, true));

  globals.def(freeStore.alloc<Sym>("null?"),
              freeStore.alloc<NatFn>(typePred<Nil>, 1, false));
  globals.def(freeStore.alloc<Sym>("pair?"),
              freeStore.alloc<NatFn>(typePred<SExprs>, 1, false));
  globals.def(freeStore.alloc<Sym>("cons"),
              freeStore.alloc<NatFn>(cons, 2, false));
  globals.def(freeStore.alloc<Sym>("car"),
              freeStore.alloc<NatFn>(car, 1, false));
  globals.def(freeStore.alloc<Sym>("cdr"),
              freeStore.alloc<NatFn>(cdr, 1, false));

  globals.def(freeStore.alloc<Sym>("dis"),
              freeStore.alloc<NatFn>(dis, 1, false));
  globals.def(freeStore.alloc<Sym>("display"),
              freeStore.alloc<NatFn>(display, 1, false));
  globals.def(freeStore.alloc<Sym>("newline"),
              freeStore.alloc<NatFn>(newline, 0, false));

  globals.def(freeStore.alloc<Sym>("quit"),
              freeStore.alloc<NatFn>(quit, 0, false));
  globals.def(freeStore.alloc<Sym>("error"),
              freeStore.alloc<NatFn>(fn::error, 1, false));

  globals.def(freeStore.alloc<Sym>("eq?"),
              freeStore.alloc<NatFn>(eq, 2, false));
  globals.def(freeStore.alloc<Sym>("eqv?"),
              freeStore.alloc<NatFn>(eqv, 2, false));
  globals.def(freeStore.alloc<Sym>("equal?"),
              freeStore.alloc<NatFn>(equal, 2, false));

  globals.def(freeStore.alloc<Sym>("procedure?"),
              freeStore.alloc<NatFn>(typePred<Closure, NatFn>, 1, false));

  globals.def(freeStore.alloc<Sym>("apply"),
              freeStore.alloc<NatFn>(apply, 2, true, true));
}

void VM::regMacro(const Sym &sym) { globals.regMacro(sym); }

bool VM::isMacro(const Sym &sym) { return globals.isMacro(sym); }

const SExpr &VM::eval(const Prototype &main) {
  const auto &res = eval(main, false);
  return res;
}

const SExpr &VM::evalWithGC(const Prototype &main) {
  const auto &res = eval(main, true);
  return res;
}
