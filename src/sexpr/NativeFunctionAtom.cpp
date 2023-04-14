#include "NativeFunctionAtom.hpp"
#include "cast.cpp"
#include <functional>
#include <memory>
#include <string>

NativeFunctionAtom::NativeFunctionAtom(NativeFn fn, const int argc)
    : Atom(SExpr::Type::NATIVE_FN), fn(fn), argc(argc) {}

std::shared_ptr<SExpr>
NativeFunctionAtom::invoke(std::vector<std::shared_ptr<SExpr>>::iterator params,
                           const unsigned int incomingArgc) {
  if (argc != -1 && incomingArgc != argc) {
    std::stringstream ss;
    ss << "Invalid number of arguments. Expected \"" << argc << "\", but got \""
       << incomingArgc << "\".";
    throw RuntimeException(ss.str());
  }
  return fn(params, incomingArgc);
}

std::string NativeFunctionAtom::toString() const { return "<Native function>"; }

bool NativeFunctionAtom::equals(const SExpr &other) const { return false; }

bool NativeFunctionAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::NATIVE_FN;
}

const std::string NativeFunctionAtom::typeName = "Native function";