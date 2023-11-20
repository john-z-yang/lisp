#include "NatFn.hpp"
#include <functional>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>

using namespace sexpr;
using namespace fn;
using namespace runtime;

std::ostream &NatFn::serialize(std::ostream &o) const {
  return o << "<Native function>";
}

bool NatFn::equals(const SExpr &other) const { return this == &other; }

NatFn::NatFn(CPPFn &fn, const uint8_t arity, const bool variadic)
    : Atom(SExpr::Type::NATIVE_FN),
      fn(fn),
      arity(arity),
      variadic(variadic),
      abandonsCont(false) {}

NatFn::NatFn(
    CPPFn &fn, const uint8_t arity, const bool variadic, const bool abandonsCont
)
    : Atom(SExpr::Type::NATIVE_FN),
      fn(fn),
      arity(arity),
      variadic(variadic),
      abandonsCont(abandonsCont) {}

const SExpr *NatFn::invoke(StackIter params, const uint8_t argc, VM &vm) const {
  if ((!variadic && argc != arity) || (variadic && argc < arity)) {
    std::stringstream ss;
    ss << "Invalid number of arguments. Expected " << unsigned(arity);
    if (variadic) {
      ss << " or more";
    }
    ss << " arguments, but got " << unsigned(argc) << ".";
    throw std::invalid_argument(ss.str());
  }
  return fn(params, argc, vm);
}

bool NatFn::classOf(const SExpr *sExpr) {
  return sExpr->type == SExpr::Type::NATIVE_FN;
}

std::string NatFn::getTypeName() { return "<Native function>"; }
