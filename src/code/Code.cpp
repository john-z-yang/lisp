#include "Code.hpp"
#include "../code/OpCode.hpp"
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
  uint16_t offset = byteCodes.size() - idx - 2;
  byteCodes[idx] = (offset >> 8) & 0xFF;
  byteCodes[idx + 1] = offset & 0xFF;
}

std::ostream &operator<<(std::ostream &o, const Code &code) {
  o << "-> consts:" << std::endl;
  for (auto i = code.consts.begin(); i != code.consts.end(); ++i) {
    if (i != code.consts.begin()) {
      o << ", ";
    }
    o << **i;
  }
  o << std::endl << "-> bytecodes (raw):" << std::endl;
  for (auto i = code.byteCodes.begin(); i != code.byteCodes.end(); ++i) {
    if (i != code.byteCodes.begin()) {
      o << " ";
    }
    o << "0x" << std::setfill('0') << std::setw(2) << std::hex << (int)*i
      << std::setfill(' ') << std::dec;
  }
  o << std::endl << "-> bytecodes:" << std::endl;

#define READ_BYTE() ((uint8_t)code.byteCodes[ip++])
#define READ_SHORT()                                                           \
  (ip += 2, (uint16_t)((code.byteCodes[ip - 2] << 8 | code.byteCodes[ip - 1])))
  std::vector<uint8_t>::size_type ip = 0;
  for (;;) {
    o << std::right << std::setw(4)
      << (code.lineNums[ip] > 0 ? std::to_string(code.lineNums[ip]) : "?")
      << std::setw(16) << ip << " " << std::setw(24) << std::left;
    uint8_t byte = READ_BYTE();
    switch (byte) {
    case OpCode::RETURN:
      return o << "RETURN" << std::endl;
    case OpCode::CALL:
      o << "CALL" << unsigned(READ_BYTE()) << std::endl;
      break;
    case OpCode::POP_TOP:
      o << "POP_TOP" << std::endl;
      break;
    case OpCode::LOAD_CONST:
      o << "LOAD_CONST" << *code.consts[READ_BYTE()] << std::endl;
      break;
    case OpCode::LOAD_SYM:
      o << "LOAD_SYM" << *code.consts[READ_BYTE()] << std::endl;
      break;
    case OpCode::DEF_SYM:
      o << "DEF_SYM" << *code.consts[READ_BYTE()] << std::endl;
      break;
    case OpCode::SET_SYM:
      o << "SET_SYM" << *code.consts[READ_BYTE()] << std::endl;
      break;
    case OpCode::LOAD_FAST:
      o << "LOAD_FAST" << unsigned(READ_BYTE()) << std::endl;
      break;
    case OpCode::SET_FAST:
      o << "SET_FAST" << unsigned(READ_BYTE()) << std::endl;
      break;
    case OpCode::JUMP:
      o << "JUMP" << unsigned(READ_SHORT()) << std::endl;
      break;
    case OpCode::POP_JUMP_IF_FALSE:
      o << "POP_JUMP_IF_FALSE" << unsigned(READ_SHORT()) << std::endl;
      break;
    default:
      break;
    }
  }
#undef READ_BYTE

  return o;
}