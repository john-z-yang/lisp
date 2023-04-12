#ifndef LISP_SRC_CODE_OPCODE_HPP_
#define LISP_SRC_CODE_OPCODE_HPP_

#include <cstdint>

enum OpCode : uint8_t {
  RETURN,
  CALL,
  POP_TOP,
  LOAD_CONST,
  LOAD_SYM,
  DEF_SYM,
  SET_SYM,
  LOAD_FAST,
  DEF_SYM_FAST,
  SET_SYM_FAST,
  JUMP,
  POP_JUMP_IF_FALSE,
  EQ,
  GT,
  GT_EQ,
  LT,
  LT_EQ,
  ABS,
  NEG,
  ADD,
  SUB,
  MULT,
  DIV,
  MOD,
};

#endif