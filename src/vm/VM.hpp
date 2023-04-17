#ifndef LISP_SRC_VM_VM_HPP_
#define LISP_SRC_VM_VM_HPP_

#include "../sexpr/ClosureAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "Env.hpp"
#include <memory>
#include <vector>

class VM {
public:
  VM();
  std::shared_ptr<SExpr> exec(std::shared_ptr<FnAtom> main);

private:
  struct CallFrame {
    std::shared_ptr<ClosureAtom> closure;
    std::vector<uint8_t>::size_type ip;
    std::vector<std::shared_ptr<SExpr>>::size_type bp;
  };

  Env globals;
  std::vector<std::shared_ptr<SExpr>> stack;
  std::vector<CallFrame> frames;

  void call(const uint8_t argc);
  std::shared_ptr<SExpr>
  peak(std::vector<std::shared_ptr<SExpr>>::size_type distance);
  std::shared_ptr<SExprs>
  makeList(std::vector<std::shared_ptr<SExpr>>::size_type size);
};

#endif
