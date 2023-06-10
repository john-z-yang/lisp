#include "VM.hpp"
#include "../common/cast.cpp"
#include "../common/sexpr/BoolAtom.hpp"
#include "../common/sexpr/ClosureAtom.hpp"
#include "../common/sexpr/NatFnAtom.hpp"
#include "../common/sexpr/NilAtom.hpp"
#include "../common/sexpr/SExprs.hpp"
#include "Upvalue.hpp"
#include <cstdint>
#include <exception>
#include <iomanip>
#include <iterator>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>

SExpr *VM::interp(FnAtom *main) {
#define CUR_FRAME() (frames.back())
#define CUR_CLOSURE() (CUR_FRAME().closure)
#define CUR_FN() (CUR_CLOSURE()->fnAtom)
#define CUR_CODE() (CUR_FN()->code)
#define BASE_PTR() (CUR_FRAME().bp)
#define INST_PTR() (CUR_FRAME().ip)

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
      &&MAKE_LIST};

  stack.push_back(alloc<ClosureAtom>(main));
  call(0);

  DISPATCH();

MAKE_CLOSURE : {
  const auto closure = alloc<ClosureAtom>(cast<FnAtom>(READ_CONST()));
  for (unsigned int i{0}; i < closure->fnAtom->numUpVals; ++i) {
    auto isLocal = READ_BYTE();
    auto idx = READ_BYTE();
    if (isLocal == 1) {
      closure->upvalues.push_back(captureUpvalue(BASE_PTR() + idx));
    } else {
      closure->upvalues.push_back(CUR_CLOSURE()->upvalues[idx]);
    }
  }
  stack.push_back(std::move(closure));
}
  DISPATCH();
CALL : {
  call(READ_BYTE());
  DISPATCH();
}
RETURN : {
  if (frames.size() == 1) {
    const auto res = stack.back();
    frames.clear();
    stack.clear();
    return res;
  }
  frames.pop_back();
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
  stack.push_back(globals.find(cast<SymAtom>(READ_CONST())));
  DISPATCH();
}
DEF_SYM : {
  globals.def(cast<SymAtom>(READ_CONST()), stack.back());
  DISPATCH();
}
SET_SYM : {
  globals.set(cast<SymAtom>(READ_CONST()), stack.back());
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
  if (!BoolAtom::toBool(stack.back())) {
    INST_PTR() += offset;
  }
  stack.pop_back();
  DISPATCH();
}
MAKE_LIST : {
  const auto n = stack.size() - BASE_PTR() - 1;
  if (n == 0) {
    stack.push_back(alloc<NilAtom>());
  } else {
    const auto list = makeList(n);
    for (std::vector<SExpr *>::size_type i{0}; i < n; ++i) {
      stack.pop_back();
    }
    stack.push_back(std::move(list));
  }
  DISPATCH();
}

#undef CUR_FRAME
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
  if (isa<ClosureAtom>(*callee)) {
    const auto closure = cast<ClosureAtom>(callee);
    closure->assertArity(argc);
    frames.push_back({closure, 0, stack.size() - argc - 1});
    return;
  }
  const auto res =
      cast<NatFnAtom>(callee)->invoke(stack.end() - argc, argc, *this);
  for (uint8_t i{0}; i < argc + 1; ++i) {
    stack.pop_back();
  }
  stack.push_back(res);
  return;
}

std::shared_ptr<Upvalue>
VM::captureUpvalue(std::vector<SExpr *>::size_type pos) {
  auto it = openUpvalues.find(pos);
  if (it != openUpvalues.end()) {
    return it->second;
  }
  openUpvalues.insert({pos, std::make_shared<Upvalue>(pos, stack)});
  return openUpvalues[pos];
}

SExpr *VM::peak(std::vector<SExpr *>::size_type distance) {
  return stack.rbegin()[distance];
}

SExprs *VM::makeList(const std::vector<SExpr *>::size_type n) {
  auto list = alloc<SExprs>();
  auto cur = list;
  for (auto i = stack.end() - n; i != stack.end(); ++i) {
    cur->first = *i;
    if (i != stack.end() - 1) {
      cur->rest = alloc<SExprs>();
      cur = cast<SExprs>(cur->rest);
    } else {
      cur->rest = alloc<NilAtom>();
    }
  }
  return list;
}

VM::VM() {
#define BIND_NATIVE_FN(sym, func, argc)                                        \
  do {                                                                         \
    globals.def(alloc<SymAtom>(sym), alloc<NatFnAtom>(&func, argc));           \
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

SExpr *VM::exec(FnAtom *main) {
  try {
    return interp(main);
  } catch (std::exception &e) {
    std::stringstream ss;
    ss << "Runtime error: " << e.what();
    const auto re = RuntimeException(ss.str(), globals, stack, frames);
    stack.clear();
    frames.clear();
    throw re;
  }
  return nullptr;
}

VM::RuntimeException::RuntimeException(const std::string &msg, Env globals,
                                       std::vector<SExpr *> stack,
                                       std::vector<CallFrame> frames)
    : _msg(msg), globals(globals), stack(stack), frames(frames) {}

const char *VM::RuntimeException::what() const noexcept { return _msg.c_str(); }

std::ostream &operator<<(std::ostream &o, const VM::RuntimeException &re) {
  std::unordered_map<SExpr *, SymAtom *> sExprSyms;
  for (const auto &p : re.globals.getSymTable()) {
    sExprSyms.insert({p.second, p.first});
  }

  const unsigned int PADDING_WIDTH = 4;
  const unsigned int IDX_WIDTH = 8;

  o << "In code object with ip: " << re.frames.back().ip << std::endl;

  o << std::setw(PADDING_WIDTH) << std::right
    << re.frames.back().closure->fnAtom->code << "Call stack:";
  for (unsigned int idx = 0; const auto &stackFrame : re.frames) {
    o << std::endl
      << std::setw(PADDING_WIDTH) << "" << std::setw(IDX_WIDTH) << std::left
      << idx << "<Closure: " << stackFrame.closure << ", ip: " << stackFrame.ip
      << ", bp: " << stackFrame.bp << ">";
    idx += 1;
    auto it = sExprSyms.find(stackFrame.closure);
    if (it != sExprSyms.end()) {
      o << " (" << *it->second << ")";
    }
  }

  o << std::endl << "Data stack:";
  for (unsigned int idx = 0; const auto &sexpr : re.stack) {
    o << std::endl
      << std::setw(PADDING_WIDTH) << "" << std::setw(IDX_WIDTH) << std::left
      << idx << *sexpr;
    idx += 1;
    auto it = sExprSyms.find(sexpr);
    if (it != sExprSyms.end()) {
      o << " (" << it->second << ")";
    }
  }
  return o << std::endl << re.what();
}

void VM::defMacro(SymAtom *sym) { globals.defMacro(sym); }

bool VM::isMacro(SymAtom *sym) { return globals.isMacro(sym); }
