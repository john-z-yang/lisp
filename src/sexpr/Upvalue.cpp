#include "../sexpr/Upvalue.hpp"
#include "Cast.cpp"
#include <utility>

using namespace sexpr;
using namespace runtime;

std::ostream &Upvalue::serialize(std::ostream &o) const {
  return o << "<" << (isOpen() ? "Open" : "Closed") << " upvalue to "
           << std::as_const(*get()) << ">";
}

bool Upvalue::equals(const SExpr &other) const {
  if (isa<Upvalue>(other)) {
    const auto &upvalue = cast<Upvalue>(other);
    return (isOpen() && upvalue.isOpen() && stackPos == upvalue.stackPos) ||
           (!isOpen() && !upvalue.isOpen() &&
            std::as_const(*ref.value()) == std::as_const(*upvalue.ref.value()));
  }
  return false;
}

Upvalue::Upvalue(const StackPtr stackPos, std::vector<sexpr::SExpr *> &stack)
    : Atom(SExpr::Type::UPVALUE), stackPos(stackPos), stack(stack) {}

bool Upvalue::isOpen() const { return !ref.has_value(); }

void Upvalue::fixupAddrs(const runtime::BreakTable &breakTable) {
  if (!isOpen()) {
    ref = breakTable.get(ref.value());
  }
}

void Upvalue::close() { ref = stack.get()[stackPos]; }

SExpr *Upvalue::get() const {
  if (isOpen()) {
    return stack.get()[stackPos];
  }
  return ref.value();
}

void Upvalue::set(SExpr *sexpr) {
  if (isOpen()) {
    stack.get()[stackPos] = sexpr;
    return;
  }
  ref = sexpr;
}

bool Upvalue::classOf(const SExpr *sExpr) {
  return sExpr->type == SExpr::Type::UPVALUE;
}

std::string Upvalue::getTypeName() { return "<Upvalue>"; }
