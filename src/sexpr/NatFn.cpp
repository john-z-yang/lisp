#include "NatFn.hpp"
#include <functional>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>

using namespace sexpr;
using namespace runtime;

NatFn::NatFn(CppFn fn, const int argc)
    : Atom(SExpr::Type::NATIVE_FN), fn(fn), argc(argc) {}

const SExpr *NatFn::invoke(std::vector<const SExpr *>::iterator params,
                           const unsigned int incomingArgc, VM &vm) const {
  if (argc != -1 && incomingArgc != (unsigned int)argc) {
    std::stringstream ss;
    ss << "Invalid number of arguments. Expected " << argc
       << " arguments, but got " << incomingArgc << ".";
    throw std::invalid_argument(ss.str());
  }
  return fn(params, incomingArgc, vm);
}

std::string NatFn::toString() const { return "<Native function>"; }

bool NatFn::equals(const SExpr &other) const { return this == &other; }

bool NatFn::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::NATIVE_FN;
}

const std::string NatFn::typeName = "<Native function>";
