#ifndef LISP_SRC_CODE_OPCODE_HPP_
#define LISP_SRC_CODE_OPCODE_HPP_

#include <cstdint>

enum OpCode : uint8_t {
  RETURN,
  CALL,
  MAKE_CLOSURE,
  POP_TOP,
  LOAD_CONST,
  LOAD_SYM,
  DEF_SYM,
  SET_SYM,
  LOAD_UPVALUE,
  SET_UPVALUE,
  LOAD_STACK,
  SET_STACK,
  JUMP,
  POP_JUMP_IF_FALSE,
};

#endif
