#ifndef LISP_SRC_VM_VM_HPP_
#define LISP_SRC_VM_VM_HPP_

#include "../sexpr/FnAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "Env.hpp"
#include <memory>
#include <vector>

class VM {
  struct CallFrame {
    std::shared_ptr<FnAtom> function;
    std::vector<uint8_t>::size_type ip;
    std::vector<std::shared_ptr<SExpr>>::size_type bp;
  };

  Env &globals;
  std::vector<std::shared_ptr<SExpr>> stack;
  std::vector<CallFrame> frames;

  void call(const uint8_t argc);
  std::shared_ptr<SExpr>
  peak(std::vector<std::shared_ptr<SExpr>>::size_type distance);

public:
  VM(std::shared_ptr<FnAtom> main, Env &globals);
  std::shared_ptr<SExpr> exec();
};

#endif