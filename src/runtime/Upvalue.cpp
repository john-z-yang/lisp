#include "Upvalue.hpp"
#include "StackPtr.hpp"

using namespace sexpr;
using namespace runtime;

Upvalue::Upvalue(
    const StackPtr stackPos,
    std::vector<std::reference_wrapper<const sexpr::SExpr>> &stack
)
    : stackPos(stackPos), stack(stack) {}

bool Upvalue::isOpen() const { return !ref.has_value(); }

void Upvalue::close() { ref = stack[stackPos].get(); }

const SExpr &Upvalue::get() const {
  if (isOpen()) {
    return stack[stackPos];
  }
  return ref->get();
}

void Upvalue::set(const SExpr &sexpr) {
  if (isOpen()) {
    stack[stackPos] = sexpr;
    return;
  }
  ref = sexpr;
}

bool runtime::operator==(const Upvalue &lhs, const Upvalue &rhs) {
  return lhs.get() == rhs.get();
}
