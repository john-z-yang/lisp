#include "Env.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/NativeFunctionAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/cast.cpp"
#include "../vm/RuntimeException.hpp"
#include <iostream>
#include <memory>

Env::Env() {
#define BIND_NATIVE_FN(sym, func, argc)                                        \
  do {                                                                         \
    symTable.insert(                                                           \
        {SymAtom(#sym), std::make_shared<NativeFunctionAtom>(&func, argc)});   \
  } while (false)

  BIND_NATIVE_FN(display, lispDisplay, 1);

  BIND_NATIVE_FN(=, lispEq, -1);
  BIND_NATIVE_FN(+, lispAdd, -1);
  BIND_NATIVE_FN(>, lispGt, -1);
  BIND_NATIVE_FN(>=, lispGteq, -1);
  BIND_NATIVE_FN(<, lispLt, -1);
  BIND_NATIVE_FN(<=, lispLteq, -1);

  BIND_NATIVE_FN(+, lispAdd, -1);
  BIND_NATIVE_FN(-, lispSub, -1);
  BIND_NATIVE_FN(*, lispMult, -1);
  BIND_NATIVE_FN(/, lispDiv, -1);

  BIND_NATIVE_FN(abs, lispAbs, 1);
  BIND_NATIVE_FN(%, lispMod, 2);

#undef BIND_NATIVE_FN
}

void Env::def(SymAtom &sym, std::shared_ptr<SExpr> val) {
  auto it = symTable.find(sym);
  if (it != symTable.end()) {
    throw RuntimeException("Symbol \"" + sym.val + "\" is already defined.");
  }
  symTable.insert({sym, val});
}

void Env::set(SymAtom &sym, std::shared_ptr<SExpr> val) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw RuntimeException("Symbol \"" + sym.val + "\" is not defined.");
  }
  symTable[sym] = val;
}

std::shared_ptr<SExpr> Env::find(SymAtom &sym) {
  auto it = symTable.find(sym);
  if (it == symTable.end()) {
    throw RuntimeException("Symbol \"" + sym.val + "\" is not defined.");
  }
  return it->second;
}

std::shared_ptr<SExpr>
lispDisplay(std::vector<std::shared_ptr<SExpr>>::iterator params,
            const uint8_t argc) {
  std::cout << **params << std::endl;
  return std::make_shared<NilAtom>();
}

#define MATH_CMP_OP(op)                                                        \
  do {                                                                         \
    const auto prev = cast<IntAtom>(*params)->val;                             \
    ++params;                                                                  \
    for (auto i = 1; i < argc; i += 1) {                                       \
      if (!(cast<IntAtom>(*params)->val op prev)) {                            \
        return std::make_shared<BoolAtom>(false);                              \
      }                                                                        \
      ++params;                                                                \
    }                                                                          \
    return std::make_shared<BoolAtom>(true);                                   \
  } while (false)

#define MATH_CUM_OP(op, init)                                                  \
  do {                                                                         \
    int res = init;                                                            \
    for (auto i = 0; i < argc; i += 1) {                                       \
      res op cast<IntAtom>(*params)->val;                                      \
      ++params;                                                                \
    }                                                                          \
    return std::make_shared<IntAtom>(res);                                     \
  } while (false)

#define MATH_DIM_OP(op, unaryOp)                                               \
  do {                                                                         \
    if (argc == 1) {                                                           \
      return std::make_shared<IntAtom>(unaryOp cast<IntAtom>(*params)->val);   \
    }                                                                          \
    int res = cast<IntAtom>(*params)->val;                                     \
    ++params;                                                                  \
    for (auto i = 1; i < argc; i += 1) {                                       \
      res op cast<IntAtom>(*params)->val;                                      \
      ++params;                                                                \
    }                                                                          \
    return std::make_shared<IntAtom>(res);                                     \
  } while (false)

std::shared_ptr<SExpr>
lispEq(std::vector<std::shared_ptr<SExpr>>::iterator params,
       const uint8_t argc) {
  MATH_CMP_OP(==);
}

std::shared_ptr<SExpr>
lispGt(std::vector<std::shared_ptr<SExpr>>::iterator params,
       const uint8_t argc) {
  MATH_CMP_OP(>);
}

std::shared_ptr<SExpr>
lispGteq(std::vector<std::shared_ptr<SExpr>>::iterator params,
         const uint8_t argc) {
  MATH_CMP_OP(>=);
}

std::shared_ptr<SExpr>
lispLt(std::vector<std::shared_ptr<SExpr>>::iterator params,
       const uint8_t argc) {
  MATH_CMP_OP(<);
}

std::shared_ptr<SExpr>
lispLteq(std::vector<std::shared_ptr<SExpr>>::iterator params,
         const uint8_t argc) {
  MATH_CMP_OP(>=);
}

std::shared_ptr<SExpr>
lispAdd(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  MATH_CUM_OP(+=, 0);
}

std::shared_ptr<SExpr>
lispMult(std::vector<std::shared_ptr<SExpr>>::iterator params,
         const uint8_t argc) {
  MATH_CUM_OP(*=, 1);
}

std::shared_ptr<SExpr>
lispSub(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  MATH_DIM_OP(-=, 0 -);
}

std::shared_ptr<SExpr>
lispDiv(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  MATH_DIM_OP(/=, 1 /);
}

std::shared_ptr<SExpr>
lispAbs(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  return std::make_shared<IntAtom>(-cast<IntAtom>(*params)->val);
}

std::shared_ptr<SExpr>
lispMod(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  const auto lhs = cast<IntAtom>(*params)->val;
  ++params;
  const auto rhs = cast<IntAtom>(*params)->val;
  return std::make_shared<IntAtom>(lhs % rhs);
}