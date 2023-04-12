#include "VM.hpp"
#include "../code/OpCode.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/cast.cpp"
#include <cstdint>

VM::VM(std::shared_ptr<FunctionAtom> main, Env &globals) : globals(globals) {
  stack.push_back(main);
  call(0);
}

void VM::call(const uint8_t argc) {
  const auto callee = cast<FunctionAtom>(peak(argc));
  if (stack.size() == 1) {
    frames.push_back({callee, 0, 0});
    return;
  }
  frames.push_back({callee, 0, stack.size() - (argc + 1)});
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
#define UNARY_MATH_OP(op, resType)                                             \
  do {                                                                         \
    auto oprand = cast<IntAtom>(stack.back())->val;                            \
    stack.pop_back();                                                          \
    stack.push_back(std::make_shared<resType>(op(oprand)));                    \
  } while (false)
#define BINARY_MATH_OP(op, resType)                                            \
  do {                                                                         \
    auto rhs = cast<IntAtom>(stack.back())->val;                               \
    stack.pop_back();                                                          \
    auto lhs = cast<IntAtom>(stack.back())->val;                               \
    stack.pop_back();                                                          \
    stack.push_back(std::make_shared<resType>(lhs op rhs));                    \
  } while (false)

  for (;;) {
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
    case OpCode::EQ: {
      BINARY_MATH_OP(==, BoolAtom);
      break;
    }
    case OpCode::GT: {
      BINARY_MATH_OP(>, BoolAtom);
      break;
    }
    case OpCode::GT_EQ: {
      BINARY_MATH_OP(>=, BoolAtom);
      break;
    }
    case OpCode::LT: {
      BINARY_MATH_OP(<, BoolAtom);
      break;
    }
    case OpCode::LT_EQ: {
      BINARY_MATH_OP(<=, BoolAtom);
      break;
    }
    case OpCode::ABS: {
      UNARY_MATH_OP(abs, IntAtom);
      break;
    }
    case OpCode::NEG: {
      UNARY_MATH_OP(-, IntAtom);
      break;
    }
    case OpCode::ADD: {
      BINARY_MATH_OP(+, IntAtom);
      break;
    }
    case OpCode::SUB: {
      BINARY_MATH_OP(-, IntAtom);
      break;
    }
    case OpCode::MULT: {
      BINARY_MATH_OP(*, IntAtom);
      break;
    }
    case OpCode::DIV: {
      BINARY_MATH_OP(/, IntAtom);
      break;
    }
    case OpCode::MOD: {
      BINARY_MATH_OP(%, IntAtom);
      break;
    }
    default:
      break;
    }
  }

  return stack.back();
#undef READ_BYTE
#undef READ_SHORT
#undef UNARY_MATH_OP
#undef BINARY_MATH_OP
}