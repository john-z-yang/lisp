#ifndef LISP_SRC_CODE_OPCODE_HPP_
#define LISP_SRC_CODE_OPCODE_HPP_

#include <cstdint>

namespace code {

enum OpCode : uint8_t {
  MAKE_CLOSURE,
  CALL,
  RETURN,
  POP_TOP,
  POP,
  CLOSE_UPVALUE,
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
  MAKE_LIST,
  MAKE_NIL,
};

} // namespace code

#endif
