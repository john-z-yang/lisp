#include "VM.hpp"
#include "../code/OpCode.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NatFnAtom.hpp"
#include "../sexpr/cast.cpp"
#include <cstdint>

VM::VM(std::shared_ptr<FnAtom> main, Env &globals) : globals(globals) {
  stack.push_back(main);
  call(0);
}

void VM::call(const uint8_t argc) {
  const auto callee = peak(argc);
  if (isa<FnAtom>(*callee)) {
    frames.push_back({cast<FnAtom>(callee), 0, stack.size() - argc - 1});
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

std::shared_ptr<SExpr> VM::exec() {

#define READ_BYTE() (curFrame.function->getCode().byteCodes[curFrame.ip++])
#define READ_SHORT()                                                           \
  (curFrame.ip += 2,                                                           \
   (uint16_t)((curFrame.function->getCode().byteCodes[curFrame.ip - 2] << 8 |  \
               curFrame.function->getCode().byteCodes[curFrame.ip - 1])))

  while (true) {
    auto &curFrame = frames.back();
    uint8_t byte = READ_BYTE();
    switch (byte) {
    case OpCode::RETURN: {
      auto res = stack.back();
      if (frames.size() == 1) {
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
    case OpCode::POP_TOP: {
      stack.pop_back();
      break;
    }
    case OpCode::LOAD_CONST: {
      stack.push_back(curFrame.function->getCode().consts[READ_BYTE()]);
      break;
    }
    case OpCode::LOAD_SYM: {
      stack.push_back(globals.find(
          *cast<SymAtom>(curFrame.function->getCode().consts[READ_BYTE()])));
      break;
    }
    case OpCode::DEF_SYM: {
      globals.def(
          *cast<SymAtom>(curFrame.function->getCode().consts[READ_BYTE()]),
          stack.back());
      break;
    }
    case OpCode::SET_SYM: {
      globals.set(
          *cast<SymAtom>(curFrame.function->getCode().consts[READ_BYTE()]),
          stack.back());
      break;
    }
    case OpCode::LOAD_FAST: {
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
    default:
      break;
    }
  }
  return stack.back();

#undef READ_BYTE
#undef READ_SHORT
}
