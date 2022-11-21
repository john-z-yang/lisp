#include "../eval/EvalException.hpp"
#include <memory>
#include <sstream>

template <typename To, typename From> bool isa(From &f) {
  return To::classOf(f);
}

template <typename To, typename From>
std::shared_ptr<To> cast(std::shared_ptr<From> f) {
  if (isa<To>(*f)) {
    return dynamic_pointer_cast<To>(f);
  }
  std::stringstream ss;
  ss << "Mismatched types. Expected \"" << To::typeName << "\", but got \""
     << *f << "\".";
  throw EvalException(ss.str());
}