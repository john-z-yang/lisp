#include "Upvalue.hpp"

Upvalue::Upvalue(const std::vector<const SExpr *>::size_type stackPos,
                 std::vector<const SExpr *> &stack)
    : stackPos(stackPos), stack(stack), value(nullptr) {}

bool Upvalue::isOpen() const { return value == nullptr; }

void Upvalue::close() { value = std::move(stack[stackPos]); }

const SExpr *Upvalue::get() const {
  if (isOpen()) {
    return stack[stackPos];
  }
  return value;
}

void Upvalue::set(const SExpr *sexpr) {
  if (isOpen()) {
    stack[stackPos] = sexpr;
  }
  value = sexpr;
}

bool operator==(const Upvalue &lhs, const Upvalue &rhs) {
  return *lhs.get() == *rhs.get();
}
