#ifndef LISP_SRC_RUNTIME_UPVALUE_HPP_
#define LISP_SRC_RUNTIME_UPVALUE_HPP_

#include "../sexpr/SExpr.hpp"
#include "StackPtr.hpp"
#include <functional>
#include <memory>
#include <optional>
#include <vector>

namespace runtime {

class Upvalue {
  friend bool operator==(const Upvalue &lhs, const Upvalue &rhs);

private:
  const StackPtr stackPos;
  std::vector<std::reference_wrapper<const sexpr::SExpr>> &stack;

  std::optional<std::reference_wrapper<const sexpr::SExpr>> ref;

  bool isOpen() const;

public:
  Upvalue(const StackPtr stackPos,
          std::vector<std::reference_wrapper<const sexpr::SExpr>> &stack);

  void close();

  const sexpr::SExpr &get() const;
  void set(const sexpr::SExpr &sexpr);
};

bool operator==(const Upvalue &lhs, const Upvalue &rhs);

} // namespace runtime

#endif
