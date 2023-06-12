#ifndef LISP_SRC_COMMON_CAST_CPP_
#define LISP_SRC_COMMON_CAST_CPP_

#include "TypeError.hpp"
#include <memory>
#include <sstream>

template <typename To, typename From> bool isa(From &f) {
  return To::classOf(f);
}

template <typename To, typename From> bool isa(From *f) {
  return To::classOf(*f);
}

template <typename To, typename From> const To *dynCast(const From *f) {
  if (isa<To>(f)) {
    return static_cast<const To *>(f);
  }
  return nullptr;
}

template <typename To, typename From> const To &cast(const From &f) {
  if (isa<To>(f)) {
    return static_cast<const To &>(f);
  }
  std::stringstream ss;
  ss << "Mismatched types. Expected " << To::typeName << ", but got " << f
     << ".";
  throw TypeError(ss.str(), To::typeName, &f);
}

template <typename To, typename From> const To *cast(const From *f) {
  if (isa<To>(f)) {
    return static_cast<const To *>(f);
  }
  std::stringstream ss;
  ss << "Mismatched types. Expected " << To::typeName << ", but got " << f
     << ".";
  throw TypeError(ss.str(), To::typeName, f);
}

#endif
