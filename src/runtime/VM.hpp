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
    std::shared_ptr<ClosureAtom> closure;
    std::vector<uint8_t>::size_type ip;
    std::vector<std::shared_ptr<SExpr>>::size_type bp;
  };

  Env globals;
  std::vector<std::shared_ptr<SExpr>> stack;
  std::vector<CallFrame> frames;
  std::unordered_map<std::vector<std::shared_ptr<SExpr>>::size_type,
                     std::shared_ptr<Upvalue>>
      openUpvalues;

  std::shared_ptr<SExpr> interp(std::shared_ptr<FnAtom> main);

  void call(const uint8_t argc);

  std::shared_ptr<Upvalue>
  captureUpvalue(std::vector<std::shared_ptr<SExpr>>::size_type pos);
  std::shared_ptr<SExpr>
  peak(std::vector<std::shared_ptr<SExpr>>::size_type distance);
  std::shared_ptr<SExprs>
  makeList(std::vector<std::shared_ptr<SExpr>>::size_type size);

public:
  class RuntimeException : public std::exception {
    friend std::ostream &operator<<(std::ostream &o,
                                    const VM::RuntimeException &re);

  private:
    std::string _msg;
    const Env globals;
    const std::vector<std::shared_ptr<SExpr>> stack;
    const std::vector<CallFrame> frames;

  public:
    RuntimeException(const std::string &msg, Env globals,
                     std::vector<std::shared_ptr<SExpr>> stack,
                     std::vector<CallFrame> frames);

    virtual const char *what() const noexcept override;
  };

  VM();
  std::shared_ptr<SExpr> exec(std::shared_ptr<FnAtom> main);

  void defMacro(SymAtom &sym);
  bool isMacro(SymAtom &sym);
};

std::ostream &operator<<(std::ostream &o, const VM::RuntimeException &re);

#endif
