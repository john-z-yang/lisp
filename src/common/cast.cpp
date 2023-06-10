#ifndef LISP_SRC_COMMON_CAST_CPP_
#define LISP_SRC_COMMON_CAST_CPP_

#include "TypeError.hpp"
#include <memory>
#include <sstream>

template <typename To, typename From> bool isa(From &f) {
  return To::classOf(f);
}

template <typename To, typename From> To *cast(From *f) {
  if (auto ptr = dynamic_cast<To *>(f)) {
    return ptr;
  }
  std::cout << "Mismatched types. Expected " << To::typeName << ", but got "
            << *f << ".";
  return nullptr;
}

#endif
