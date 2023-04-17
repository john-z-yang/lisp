#include "VM.hpp"
#include "../code/OpCode.hpp"
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
  stack.push_back(std::make_shared<ClosureAtom>(main));
  call(0);

#define READ_BYTE() (curFrame.closure->fnAtom->code.byteCodes[curFrame.ip++])
#define READ_SHORT()                                                           \
  (curFrame.ip += 2,                                                           \
   (uint16_t)((curFrame.closure->fnAtom->code.byteCodes[curFrame.ip - 2]       \
                   << 8 |                                                      \
               curFrame.closure->fnAtom->code.byteCodes[curFrame.ip - 1])))
#define READ_CONST() (curFrame.closure->fnAtom->code.consts[READ_BYTE()])

  while (true) {
    auto &curFrame = frames.back();
    uint8_t byte = READ_BYTE();
    switch (byte) {
    case OpCode::RETURN: {
      auto res = stack.back();
      if (frames.size() == 1) {
        frames.clear();
        stack.clear();
        return res;
      }
      while (stack.size() > frames.back().bp) {
        stack.pop_back();
      }
      frames.pop_back();
      stack.push_back(res);
      break;
    }
    case OpCode::CALL: {
      const auto argc = READ_BYTE();
      call(argc);
      break;
    }
    case OpCode::MAKE_CLOSURE: {
      auto closure = std::make_shared<ClosureAtom>(cast<FnAtom>(READ_CONST()));
      for (auto i = 0; i < closure->fnAtom->numUpVals; i++) {
        auto isLocal = READ_BYTE();
        auto idx = READ_BYTE();
        if (isLocal == 1) {
          closure->upValues.push_back(stack[curFrame.bp + idx]);
        } else {
          closure->upValues.push_back(curFrame.closure->upValues[idx]);
        }
      }
      stack.push_back(closure);
      break;
    }
    case OpCode::POP_TOP: {
      stack.pop_back();
      break;
    }
    case OpCode::LOAD_CONST: {
      stack.push_back(READ_CONST());
      break;
    }
    case OpCode::LOAD_SYM: {
      stack.push_back(globals.find(*cast<SymAtom>(READ_CONST())));
      break;
    }
    case OpCode::DEF_SYM: {
      globals.def(*cast<SymAtom>(READ_CONST()), stack.back());
      break;
    }
    case OpCode::SET_SYM: {
      globals.set(*cast<SymAtom>(READ_CONST()), stack.back());
      break;
    }
    case OpCode::LOAD_UPVALUE: {
      stack.push_back(curFrame.closure->upValues[READ_BYTE()]);
      break;
    }
    case OpCode::SET_UPVALUE: {
      curFrame.closure->upValues[READ_BYTE()] = stack.back();
      break;
    }
    case OpCode::LOAD_STACK: {
      stack.push_back(stack[curFrame.bp + READ_BYTE()]);
      break;
    }
    case OpCode::JUMP: {
      curFrame.ip += READ_SHORT();
      break;
    }
    case OpCode::POP_JUMP_IF_FALSE: {
      auto offset = READ_SHORT();
      if (!BoolAtom::toBool(stack.back())) {
        curFrame.ip += offset;
      }
      stack.pop_back();
      break;
    }
    case OpCode::MAKE_VAR_ARGS: {
      const auto n = stack.size() - curFrame.bp - 1;
      if (n == 0) {
        stack.push_back(std::make_shared<NilAtom>());
        break;
      }
      const auto list = makeList(n);
      for (auto i = 0; i < n; i++) {
        stack.pop_back();
      }
      stack.push_back(list);
      break;
    }
    default:
      break;
    }
  }
  return stack.back();

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONST
}

void VM::call(const uint8_t argc) {
  const auto callee = peak(argc);
  if (isa<ClosureAtom>(*callee)) {
    frames.push_back({cast<ClosureAtom>(callee), 0, stack.size() - argc - 1});
    return;
  }
  if (isa<NatFnAtom>(*callee)) {
    const auto res = cast<NatFnAtom>(callee)->invoke(stack.end() - argc, argc);
    for (auto i = 0; i < argc + 1; i += 1) {
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
