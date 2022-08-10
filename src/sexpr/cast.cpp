#include <memory>

using std::shared_ptr;

template <typename To, typename From> bool isa(const From &f) {
  return To::classOf(f);
}