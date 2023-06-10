#ifndef LISP_SRC_VM_VM_HPP_
#define LISP_SRC_VM_VM_HPP_

#include "../common/sexpr/ClosureAtom.hpp"
#include "../common/sexpr/SExpr.hpp"
#include "../common/sexpr/SExprs.hpp"
#include "Env.hpp"
#include "Upvalue.hpp"
#include <memory>
#include <unordered_map>
#include <vector>

class VM {
private:
  struct CallFrame {
    ClosureAtom *closure;
    std::vector<uint8_t>::size_type ip;
    std::vector<SExpr *>::size_type bp;
  };

  Env globals;
  std::vector<SExpr *> stack;
  std::vector<std::unique_ptr<SExpr>> heap;
  std::vector<CallFrame> frames;
  std::unordered_map<std::vector<SExpr *>::size_type, std::shared_ptr<Upvalue>>
      openUpvalues;

  SExpr *interp(FnAtom *main);

  void call(const uint8_t argc);

  std::shared_ptr<Upvalue> captureUpvalue(std::vector<SExpr *>::size_type pos);
  SExpr *peak(std::vector<SExpr *>::size_type distance);
  SExprs *makeList(std::vector<SExpr *>::size_type size);

public:
  class RuntimeException : public std::exception {
    friend std::ostream &operator<<(std::ostream &o,
                                    const VM::RuntimeException &re);

  private:
    std::string _msg;
    const Env globals;
    const std::vector<SExpr *> stack;
    const std::vector<CallFrame> frames;

  public:
    RuntimeException(const std::string &msg, Env globals,
                     std::vector<SExpr *> stack, std::vector<CallFrame> frames);

    virtual const char *what() const noexcept override;
  };

  VM();
  SExpr *exec(FnAtom *main);

  void defMacro(SymAtom &sym);
  bool isMacro(SymAtom &sym);

  template <typename T, typename... Args> T *alloc(Args &&...args) {
    heap.emplace_back(std::make_unique<T>(std::forward<Args>(args)...));
    return static_cast<T *>(heap.back().get());
  }
};

std::ostream &operator<<(std::ostream &o, const VM::RuntimeException &re);

#endif
