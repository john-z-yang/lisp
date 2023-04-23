#include "Code.hpp"
#include "OpCode.hpp"
#include "cast.cpp"
#include "sexpr/FnAtom.hpp"
#include <cstdint>
#include <iomanip>

uint8_t Code::pushCode(const uint8_t code) { return pushCode(code, 0); }

uint8_t Code::pushCode(const uint8_t code, const unsigned int lineNum) {
  byteCodes.push_back(code);
  lineNums.push_back(lineNum);
  return byteCodes.size() - 1;
}

uint8_t Code::pushConst(std::shared_ptr<SExpr> sExpr) {
  consts.push_back(sExpr);
  return consts.size() - 1;
}

void Code::patchJump(const std::vector<uint8_t>::size_type idx) {
  const uint16_t offset = byteCodes.size() - idx - 2;
  byteCodes[idx] = (offset >> 8) & 0xFF;
  byteCodes[idx + 1] = offset & 0xFF;
}

std::ostream &operator<<(std::ostream &o, const Code &code) {
#define READ_BYTE() ((uint8_t)code.byteCodes[ip++])
#define READ_SHORT()                                                           \
  (ip += 2, (uint16_t)((code.byteCodes[ip - 2] << 8 | code.byteCodes[ip - 1])))
#define DIS_NO_OPRAND_OP(name)                                                 \
  do {                                                                         \
    o << #name;                                                                \
  } while (false)
#define DIS_BYTE_OPRAND_OP(name)                                               \
  do {                                                                         \
    o << #name << unsigned(READ_BYTE());                                       \
  } while (false)
#define DIS_SHORT_OPRAND_OP(name)                                              \
  do {                                                                         \
    o << #name << unsigned(READ_SHORT());                                      \
  } while (false)
#define DIS_CONST_OP(name)                                                     \
  do {                                                                         \
    o << #name << *code.consts[READ_BYTE()];                                   \
  } while (false)

  const unsigned int PADDING_WIDTH = 4;
  o << "Constants:" << std::endl << std::setw(PADDING_WIDTH) << "";
  for (auto i = code.consts.begin(); i != code.consts.end(); ++i) {
    if (i != code.consts.begin()) {
      o << ", ";
    }
    o << **i;
  }
  o << std::endl
    << "Bytecodes (raw):" << std::endl
    << std::setw(PADDING_WIDTH) << "";
  for (auto i = code.byteCodes.begin(); i != code.byteCodes.end(); ++i) {
    if (i != code.byteCodes.begin()) {
      o << " ";
    }
    o << std::setw(2) << std::setfill('0') << std::right << std::hex
      << unsigned(*i) << std::setfill(' ') << std::dec;
  }
  o << std::endl << "Bytecodes:" << std::endl;

  const unsigned int LINE_NUM_WIDTH = 4;
  const unsigned int IP_WIDTH = 16;
  const unsigned int OP_WIDTH = 24;

  std::vector<uint8_t>::size_type ip = 0;

  while (true) {
    o << std::setw(PADDING_WIDTH) << "" << std::setw(LINE_NUM_WIDTH)
      << std::left
      << (code.lineNums[ip] > 0 ? std::to_string(code.lineNums[ip]) : "?")
      << std::setw(IP_WIDTH) << std::right << ip << " " << std::setw(OP_WIDTH)
      << std::left;

    uint8_t byte = READ_BYTE();

    switch (byte) {
    case OpCode::MAKE_CLOSURE: {
      const auto fn = cast<FnAtom>(code.consts[READ_BYTE()]);
      o << "MAKE_CLOSURE" << *fn;
      for (unsigned int i{0}; i < fn->numUpVals; i++) {
        const auto isLocal = unsigned(READ_BYTE());
        const auto idx = unsigned(READ_BYTE());
        o << std::endl
          << std::setw(PADDING_WIDTH) << "" << std::right
          << std::setw(LINE_NUM_WIDTH + IP_WIDTH + OP_WIDTH + 1);
        o << "" << (isLocal == 1 ? "LOCAL" : "UPVAL") << " " << idx;
      }
      break;
    }
    case OpCode::CALL: {
      DIS_BYTE_OPRAND_OP(CALL);
      break;
    }
    case OpCode::RETURN: {
      return o << "RETURN" << std::endl;
    }
    case OpCode::POP_TOP: {
      DIS_NO_OPRAND_OP(POP_TOP);
      break;
    }
    case OpCode::LOAD_CONST: {
      DIS_CONST_OP(LOAD_CONST);
      break;
    }
    case OpCode::LOAD_SYM: {
      DIS_CONST_OP(LOAD_SYM);
      break;
    }
    case OpCode::DEF_SYM: {
      DIS_CONST_OP(DEF_SYM);
      break;
    }
    case OpCode::SET_SYM: {
      DIS_CONST_OP(SET_SYM);
      break;
    }
    case OpCode::LOAD_UPVALUE: {
      DIS_BYTE_OPRAND_OP(LOAD_UPVALUE);
      break;
    }
    case OpCode::SET_UPVALUE: {
      DIS_BYTE_OPRAND_OP(SET_UPVALUE);
      break;
    }
    case OpCode::LOAD_STACK: {
      DIS_BYTE_OPRAND_OP(LOAD_STACK);
      break;
    }
    case OpCode::SET_STACK: {
      DIS_BYTE_OPRAND_OP(SET_STACK);
      break;
    }
    case OpCode::JUMP: {
      DIS_SHORT_OPRAND_OP(JUMP);
      break;
    }
    case OpCode::POP_JUMP_IF_FALSE: {
      DIS_SHORT_OPRAND_OP(POP_JUMP_IF_FALSE);
      break;
    }
    case OpCode::MAKE_VAR_ARGS: {
      DIS_NO_OPRAND_OP(MAKE_VAR_ARGS);
      break;
    }
    default:
      break;
    }
    o << std::endl;
  }
  return o;
#undef READ_BYTE
#undef READ_SHORT
#undef DIS_NO_OPRAND_OP
#undef DIS_BYTE_OPRAND_OP
#undef DIS_SHORT_OPRAND_OP
#undef DIS_CONST_OP
}
