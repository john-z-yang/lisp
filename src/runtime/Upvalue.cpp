#include "Upvalue.hpp"

Upvalue::Upvalue(const std::vector<SExpr *>::size_type stackPos,
                 std::vector<SExpr *> &stack)
    : stackPos(stackPos), stack(stack), value(nullptr) {}

bool Upvalue::isOpen() const { return value == nullptr; }

void Upvalue::close() { value = std::move(stack[stackPos]); }

SExpr *Upvalue::get() const {
  if (isOpen()) {
    return stack[stackPos];
  }
  return value;
}

void Upvalue::set(SExpr *sexpr) {
  if (isOpen()) {
    stack[stackPos] = sexpr;
  }
  value = sexpr;
}

bool operator==(const Upvalue &lhs, const Upvalue &rhs) {
  return *lhs.get() == *rhs.get();
}
