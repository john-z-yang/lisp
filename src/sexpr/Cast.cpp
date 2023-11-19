#ifndef LISP_SRC_SEXPR_CAST_CPP_
#define LISP_SRC_SEXPR_CAST_CPP_

#include "../error/TypeError.hpp"
#include "SExpr.hpp"
#include <memory>
#include <sstream>

namespace sexpr {

template <typename T> bool isa(const SExpr &f) { return T::classOf(f); }

template <typename T> void assertType(const SExpr &f) {
  if (!isa<T>(f)) {
    std::stringstream ss;
    ss << "Mismatched types. Expected " << T::getTypeName() << ", but got " << f
       << ".";
    throw error::TypeError(ss.str(), T::getTypeName(), f);
  }
}

template <typename First, typename Next, typename... Rest>
void assertType(const SExpr &f) {
  if (!(isa<First>(f) || isa<Next>(f) || (isa<Rest>(f) || ...))) {
    std::string typeName =
        "one of: (" + First::getTypeName() + ", " + Next::getTypeName();
    ([&typeName] { typeName += ", " + Rest::getTypeName(); }(), ...);
    typeName += ")";
    std::stringstream ss;
    ss << "Mismatched types. Expected " << typeName << ", but got " << f << ".";
    throw error::TypeError(ss.str(), typeName, f);
  }
}

template <typename T> const T &cast(const SExpr &f) {
  assertType<T>(f);
  return static_cast<const T &>(f);
}

} // namespace sexpr

#endif