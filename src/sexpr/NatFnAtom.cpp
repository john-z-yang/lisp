#include "NatFnAtom.hpp"
#include "../vm/RuntimeException.hpp"
#include <functional>
#include <memory>
#include <sstream>
#include <string>

NatFnAtom::NatFnAtom(NativeFn fn, const int argc)
    : Atom(SExpr::Type::NATIVE_FN), fn(fn), argc(argc) {}

std::shared_ptr<SExpr>
NatFnAtom::invoke(std::vector<std::shared_ptr<SExpr>>::iterator params,
                  const unsigned int incomingArgc) {
  if (argc != -1 && incomingArgc != (unsigned int)argc) {
    std::stringstream ss;
    ss << "Invalid number of arguments. Expected \"" << argc << "\", but got \""
       << incomingArgc << "\".";
    throw RuntimeException(ss.str());
  }
  return fn(params, incomingArgc);
}

std::string NatFnAtom::toString() const { return "<Native function>"; }

bool NatFnAtom::equals(const SExpr &other) const { return false; }

bool NatFnAtom::classOf(const SExpr &sExpr) {
  return sExpr.type == SExpr::Type::NATIVE_FN;
}

const std::string NatFnAtom::typeName = "<Native function>";
