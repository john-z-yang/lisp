#include "../error/RuntimeError.hpp"
#include "../sexpr/NatFn.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/cast.cpp"
#include "VM.hpp"
#include <sstream>

using namespace sexpr;
using namespace runtime;
using namespace error;

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

#define GC_ATOMIC(stmts)                                                       \
  do {                                                                         \
                                                                               \
  } while (false)

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
  auto it = openUpvalues.find(stack.size() - 1);
  if (it != openUpvalues.end()) {
    it->second->close();
    openUpvalues.erase(it);
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

std::shared_ptr<Upvalue>
VM::captureUpvalue(std::vector<const SExpr *>::size_type pos) {
  auto it = openUpvalues.find(pos);
  if (it != openUpvalues.end()) {
    return it->second;
  }
  openUpvalues.insert({pos, std::make_shared<Upvalue>(pos, stack)});
  return openUpvalues[pos];
}

const SExpr *VM::peak(std::vector<const SExpr *>::size_type distance) {
  return stack.rbegin()[distance];
}

const SExpr *VM::makeList(const std::vector<const SExpr *>::size_type n) {
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
