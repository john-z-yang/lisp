#ifndef LISP_SRC_VM_ENV_HPP_
#define LISP_SRC_VM_ENV_HPP_

#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SymAtom.hpp"
#include "../vm/RuntimeException.hpp"
#include <memory>
#include <unordered_map>

class Env {
  typedef std::unordered_map<SymAtom, std::shared_ptr<SExpr>,
                             SymAtom::HashFunction>
      SymVals;

private:
  SymVals symTable;

public:
  Env();

  void def(SymAtom &sym, std::shared_ptr<SExpr> val);

  void set(SymAtom &sym, std::shared_ptr<SExpr> val);

  std::shared_ptr<SExpr> find(SymAtom &sym);
};

std::shared_ptr<BoolAtom>
lispEq(std::vector<std::shared_ptr<SExpr>>::iterator params,
       const uint8_t argc);

std::shared_ptr<IntAtom>
lispAdd(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc);

std::shared_ptr<IntAtom>
lispSub(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc);

std::shared_ptr<IntAtom>
lispMult(std::vector<std::shared_ptr<SExpr>>::iterator params,
         const uint8_t argc);

std::shared_ptr<IntAtom>
lispDiv(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc);

std::shared_ptr<IntAtom>
lispMod(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc);

#endif