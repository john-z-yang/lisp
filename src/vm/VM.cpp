#include "VM.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/ClosureAtom.hpp"
#include "../sexpr/NatFnAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/cast.cpp"
#include <cstdint>
#include <iterator>
#include <memory>

VM::VM() {}

std::shared_ptr<SExpr> VM::exec(std::shared_ptr<FnAtom> main) {
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

  static void *dispatchTable[] = {&&MAKE_CLOSURE, &&CALL,
                                  &&RETURN,       &&POP_TOP,
                                  &&LOAD_CONST,   &&LOAD_SYM,
                                  &&DEF_SYM,      &&SET_SYM,
                                  &&LOAD_UPVALUE, &&SET_UPVALUE,
                                  &&LOAD_STACK,   &&SET_STACK,
                                  &&JUMP,         &&POP_JUMP_IF_FALSE,
                                  &&MAKE_VAR_ARGS};

  stack.push_back(std::make_shared<ClosureAtom>(main));
  call(0);

  DISPATCH();

MAKE_CLOSURE : {
  const auto closure =
      std::make_shared<ClosureAtom>(cast<FnAtom>(READ_CONST()));
  for (unsigned int i{0}; i < closure->fnAtom->numUpVals; ++i) {
    auto isLocal = READ_BYTE();
    auto idx = READ_BYTE();
    if (isLocal == 1) {
      closure->upValues.push_back(stack[BASE_PTR() + idx]);
    } else {
      closure->upValues.push_back(CUR_CLOSURE()->upValues[idx]);
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
  auto res = std::move(stack.back());
  if (frames.size() == 1) {
    frames.clear();
    stack.clear();
    return res;
  }
  while (stack.size() > frames.back().bp) {
    stack.pop_back();
  }
  frames.pop_back();
  stack.push_back(std::move(res));
}
  DISPATCH();
POP_TOP : {
  stack.pop_back();
  DISPATCH();
}
LOAD_CONST : {
  stack.push_back(READ_CONST());
  DISPATCH();
}
LOAD_SYM : {
  stack.push_back(globals.find(*cast<SymAtom>(READ_CONST())));
  DISPATCH();
}
DEF_SYM : {
  globals.def(*cast<SymAtom>(READ_CONST()), stack.back());
  DISPATCH();
}
SET_SYM : {
  globals.set(*cast<SymAtom>(READ_CONST()), stack.back());
  DISPATCH();
}
LOAD_UPVALUE : {
  stack.push_back(CUR_CLOSURE()->upValues[READ_BYTE()]);
  DISPATCH();
}
SET_UPVALUE : {
  CUR_CLOSURE()->upValues[READ_BYTE()] = stack.back();
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
MAKE_VAR_ARGS : {
  const auto n = stack.size() - BASE_PTR() - 1;
  if (n == 0) {
    stack.push_back(std::make_shared<NilAtom>());
  } else {
    const auto list = makeList(n);
    for (std::vector<std::shared_ptr<SExpr>>::size_type i{0}; i < n; ++i) {
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
    frames.push_back({cast<ClosureAtom>(callee), 0, stack.size() - argc - 1});
    return;
  }
  if (isa<NatFnAtom>(*callee)) {
    const auto res = cast<NatFnAtom>(callee)->invoke(stack.end() - argc, argc);
    for (uint8_t i{0}; i < argc + 1; ++i) {
      stack.pop_back();
    }
    stack.push_back(res);
    return;
  }
}

std::shared_ptr<SExpr>
VM::peak(std::vector<std::shared_ptr<SExpr>>::size_type distance) {
  return stack.rbegin()[distance];
}

std::shared_ptr<SExprs>
VM::makeList(const std::vector<std::shared_ptr<SExpr>>::size_type n) {
  auto list = std::make_shared<SExprs>();
  auto cur = list;
  for (auto i = stack.end() - n; i != stack.end(); ++i) {
    cur->first = *i;
    if (i != stack.end() - 1) {
      cur->rest = std::make_shared<SExprs>();
      cur = cast<SExprs>(cur->rest);
    } else {
      cur->rest = std::make_shared<NilAtom>();
    }
  }
  return list;
}
