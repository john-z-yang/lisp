#include "Upvalue.hpp"

Upvalue::Upvalue(const std::vector<std::shared_ptr<SExpr>>::size_type stackPos,
                 std::vector<std::shared_ptr<SExpr>> &stack)
    : stackPos(stackPos), stack(stack) {}

bool Upvalue::isOpen() const { return value == nullptr; }

void Upvalue::close() { value = std::move(stack[stackPos]); }

std::shared_ptr<SExpr> Upvalue::get() const {
  if (isOpen()) {
    return stack[stackPos];
  }
  return value;
}

void Upvalue::set(std::shared_ptr<SExpr> &sexpr) {
  if (isOpen()) {
    stack[stackPos] = sexpr;
  }
  value = sexpr;
}

bool operator==(const Upvalue &lhs, const Upvalue &rhs) {
  return *lhs.get() == *rhs.get();
}
