#ifndef LISP_SRC_COMMON_OPCODE_HPP_
#define LISP_SRC_COMMON_OPCODE_HPP_

#include <cstdint>

enum OpCode : uint8_t {
  MAKE_CLOSURE,
  CALL,
  RETURN,
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
  MAKE_VAR_ARGS,
};

#endif
