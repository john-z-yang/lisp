#ifndef LISP_SRC_SEXPR_CAST_CPP_
#define LISP_SRC_SEXPR_CAST_CPP_

#include "../error/TypeError.hpp"
#include <memory>
#include <sstream>

namespace sexpr {

template <typename To, typename From> bool isa(From &f) {
  return To::classOf(f);
}

template <typename To, typename From> const To &cast(const From &f) {
  if (isa<To>(f)) {
    return static_cast<const To &>(f);
  }
  std::stringstream ss;
  ss << "Mismatched types. Expected " << To::typeName << ", but got " << f
     << ".";
  throw error::TypeError(ss.str(), To::typeName, f);
}

} // namespace sexpr

#endif
