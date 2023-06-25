#include "NatFn.hpp"
#include <functional>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>

using namespace sexpr;
using namespace runtime;

std::string NatFn::toString() const { return "<Native function>"; }

bool NatFn::equals(const SExpr &other) const { return this == &other; }

NatFn::NatFn(CppFn fn, const uint8_t argc, const bool isVariadic)
    : Atom(SExpr::Type::NATIVE_FN), fn(fn), argc(argc), isVariadic(isVariadic) {
}

const SExpr &NatFn::invoke(StackIter params, const uint8_t incomingArgc,
                           VM &vm) const {
  if ((!isVariadic && incomingArgc != argc) ||
      (isVariadic && incomingArgc < argc)) {
    std::stringstream ss;
    ss << "Invalid number of arguments. Expected " << unsigned(argc);
    if (isVariadic) {
      ss << " or more";
    }
    ss << " arguments, but got " << unsigned(incomingArgc) << ".";
    throw std::invalid_argument(ss.str());
  }
  return fn(params, incomingArgc, vm);
}

bool NatFn::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::NATIVE_FN;
}
