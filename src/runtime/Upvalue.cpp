#include "Upvalue.hpp"
#include "StackPtr.hpp"

using namespace sexpr;
using namespace runtime;

Upvalue::Upvalue(const StackPtr stackPos,
                 std::vector<std::reference_wrapper<const sexpr::SExpr>> &stack)
    : stackPos(stackPos), stack(stack), value(nullptr) {}

bool Upvalue::isOpen() const { return value == nullptr; }

void Upvalue::close() { value = &stack[stackPos].get(); }

const SExpr &Upvalue::get() const {
  if (isOpen()) {
    return stack[stackPos];
  }
  return *value;
}

void Upvalue::set(const SExpr &sexpr) {
  if (isOpen()) {
    stack[stackPos] = sexpr;
    return;
  }
  value = &sexpr;
}

bool runtime::operator==(const Upvalue &lhs, const Upvalue &rhs) {
  return lhs.get() == rhs.get();
}
