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
  DEF_SYM = 0x8,
  SET_SYM = 0x9,
  LOAD_UPVALUE = 0xa,
  SET_UPVALUE = 0xb,
  LOAD_STACK = 0xc,
  SET_STACK = 0xd,
  JUMP = 0xe,
  POP_JUMP_IF_FALSE = 0xf,
  MAKE_LIST = 0x10,
  MAKE_NIL = 0x11,
};

}

#endif
