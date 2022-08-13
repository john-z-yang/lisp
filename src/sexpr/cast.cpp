#include "../repl/except/EvalException.hpp"
#include <memory>
#include <sstream>

using std::dynamic_pointer_cast;
using std::shared_ptr;
using std::stringstream;

template <typename To, typename From> bool isa(From &f) {
  return To::classOf(f);
}

template <typename To, typename From> shared_ptr<To> cast(shared_ptr<From> f) {
  if (isa<To>(*f)) {
    return dynamic_pointer_cast<To>(f);
  }
  stringstream ss;
  ss << "Mismatched types. Expected \"" << To::typeName << "\", but got \""
     << *f << "\".";
  throw EvalException(ss.str());
}