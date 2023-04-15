#include "../vm/RuntimeException.hpp"
#include <memory>
#include <sstream>

template <typename To, typename From> bool isa(From &f) {
  return To::classOf(f);
}

template <typename To, typename From>
std::shared_ptr<To> cast(std::shared_ptr<From> f) {
  if (auto ptr = std::dynamic_pointer_cast<To>(f)) {
    return ptr;
  }
  std::stringstream ss;
  ss << "Mismatched types. Expected \"" << To::typeName << "\", but got \""
     << *f << "\".";
  throw RuntimeException(ss.str());
}
