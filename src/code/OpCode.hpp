#ifndef LISP_SRC_CODE_OPCODE_HPP_
#define LISP_SRC_CODE_OPCODE_HPP_

#include <cstdint>

namespace code {

enum OpCode : uint8_t {
  MAKE_CLOSURE = 0x0,
  CALL = 0x1,
  RETURN = 0x2,
  POP_TOP = 0x3,
  POP = 0x4,
  CLOSE_UPVALUE = 0x5,
  LOAD_CONST = 0x6,
  LOAD_SYM = 0x7,
  DEF_MACRO = 0x8,
  DEF_SYM = 0x9,
  SET_SYM = 0xa,
  LOAD_UPVALUE = 0xb,
  SET_UPVALUE = 0xc,
  LOAD_STACK = 0xd,
  SET_STACK = 0xe,
  JUMP = 0xf,
  POP_JUMP_IF_FALSE = 0x10,
  MAKE_LIST = 0x11,
  MAKE_NIL = 0x12,
};

}

#endif
