#ifndef LISP_SRC_VM_UPVALUE_HPP_
#define LISP_SRC_VM_UPVALUE_HPP_

#include "../common/sexpr/SExpr.hpp"
#include <memory>

class Upvalue {
  friend bool operator==(const Upvalue &lhs, const Upvalue &rhs);

private:
  const std::vector<std::shared_ptr<SExpr>>::size_type stackPos;
  std::vector<std::shared_ptr<SExpr>> &stack;

  std::shared_ptr<SExpr> value;

  bool isOpen() const;

public:
  Upvalue(const std::vector<std::shared_ptr<SExpr>>::size_type stackPos,
          std::vector<std::shared_ptr<SExpr>> &stack);

  void close();

  std::shared_ptr<SExpr> get() const;
  void set(std::shared_ptr<SExpr> &sexpr);
};

#endif
