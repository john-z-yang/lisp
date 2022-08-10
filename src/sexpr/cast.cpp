#include "../../include/repl/EvalException.hpp"
#include <memory>

using std::dynamic_pointer_cast;
using std::shared_ptr;

template <typename To, typename From> bool isa(From &f) {
  return To::classOf(f);
}

template <typename To, typename From> shared_ptr<To> cast(shared_ptr<From> f) {
  if (To::classOf(*f)) {
    return dynamic_pointer_cast<To>(f);
  }
  throw EvalException(f->toString() + " is not a " + typeid(To).name());
}