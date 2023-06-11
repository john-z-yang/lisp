#ifndef LISP_SRC_VM_VM_HPP_
#define LISP_SRC_VM_VM_HPP_

#include "../common/sexpr/BoolAtom.hpp"
#include "../common/sexpr/ClosureAtom.hpp"
#include "../common/sexpr/NilAtom.hpp"
#include "../common/sexpr/SExpr.hpp"
#include "Env.hpp"
#include "Upvalue.hpp"
#include <memory>
#include <unordered_map>
#include <vector>

class VM {
private:
  struct CallFrame {
    const ClosureAtom *closure;
    std::vector<uint8_t>::size_type ip;
    std::vector<const SExpr *>::size_type bp;
  };

  Env globals;
  std::vector<const SExpr *> stack;
  std::vector<std::unique_ptr<const SExpr>> heap;
  std::unordered_map<int, const SExpr *> integers;
  std::vector<CallFrame> frames;
  std::unordered_map<std::vector<const SExpr *>::size_type,
                     std::shared_ptr<Upvalue>>
      openUpvalues;

  const SExpr *interp(const FnAtom *main);

  void call(const uint8_t argc);

  std::shared_ptr<Upvalue>
  captureUpvalue(std::vector<const SExpr *>::size_type pos);
  const SExpr *peak(std::vector<const SExpr *>::size_type distance);
  const SExpr *makeList(std::vector<const SExpr *>::size_type size);

public:
  class RuntimeException : public std::exception {
    friend std::ostream &operator<<(std::ostream &o,
                                    const VM::RuntimeException &re);

  private:
    std::string _msg;
    const Env globals;
    const std::vector<const SExpr *> stack;
    const std::vector<CallFrame> frames;

  public:
    RuntimeException(const std::string &msg, Env globals,
                     std::vector<const SExpr *> stack,
                     std::vector<CallFrame> frames);

    virtual const char *what() const noexcept override;
  };

  VM();
  const SExpr *exec(const FnAtom *main);

  void defMacro(const SymAtom *sym);
  bool isMacro(const SymAtom *sym);

  template <typename T, typename... Args> const T *alloc(Args &&...args) {
    heap.emplace_back(std::make_unique<const T>(std::forward<Args>(args)...));
    return static_cast<const T *>(heap.back().get());
  }
};

template <> inline const NilAtom *VM::alloc() { return NilAtom::getInstance(); }
template <> inline const BoolAtom *VM::alloc(bool &&val) {
  return BoolAtom::getInstance(val);
}

std::ostream &operator<<(std::ostream &o, const VM::RuntimeException &re);

#endif
