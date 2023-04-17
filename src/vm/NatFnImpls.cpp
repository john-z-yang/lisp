#include "NatFnImpls.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/StringAtom.hpp"
#include "../sexpr/SymAtom.hpp"
#include "../sexpr/cast.cpp"
#include <memory>

#define MATH_CMP_OP(name, op)                                                  \
  std::shared_ptr<SExpr> name(                                                 \
      std::vector<std::shared_ptr<SExpr>>::iterator params,                    \
      const uint8_t argc) {                                                    \
    const auto prev = cast<IntAtom>(*params)->val;                             \
    ++params;                                                                  \
    for (auto i = 1; i < argc; i += 1) {                                       \
      if (!(cast<IntAtom>(*params)->val op prev)) {                            \
        return std::make_shared<BoolAtom>(false);                              \
      }                                                                        \
      ++params;                                                                \
    }                                                                          \
    return std::make_shared<BoolAtom>(true);                                   \
  }

#define MATH_CUM_OP(name, op, init)                                            \
  std::shared_ptr<SExpr> name(                                                 \
      std::vector<std::shared_ptr<SExpr>>::iterator params,                    \
      const uint8_t argc) {                                                    \
    int res = init;                                                            \
    for (auto i = 0; i < argc; i += 1) {                                       \
      res op cast<IntAtom>(*params)->val;                                      \
      ++params;                                                                \
    }                                                                          \
    return std::make_shared<IntAtom>(res);                                     \
  }

#define MATH_DIM_OP(name, op, unaryOp)                                         \
  std::shared_ptr<SExpr> name(                                                 \
      std::vector<std::shared_ptr<SExpr>>::iterator params,                    \
      const uint8_t argc) {                                                    \
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
  }

#define PRED_OP(name, cond)                                                    \
  std::shared_ptr<SExpr> name(                                                 \
      std::vector<std::shared_ptr<SExpr>>::iterator params,                    \
      const uint8_t argc) {                                                    \
    return std::make_shared<BoolAtom>(cond);                                   \
  }

PRED_OP(lispIsSym, isa<SymAtom>(**params));

PRED_OP(lispIsNum, isa<IntAtom>(**params));
MATH_CMP_OP(lispEq, ==);
MATH_CMP_OP(lispGt, >);
MATH_CMP_OP(lispGteq, >=);
MATH_CMP_OP(lispLt, <);
MATH_CMP_OP(lispLteq, <=);
MATH_CUM_OP(lispAdd, +=, 0);
MATH_CUM_OP(lispMult, *=, 1);
MATH_DIM_OP(lispSub, -=, 0 -);
MATH_DIM_OP(lispDiv, /=, 1 /);

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

PRED_OP(lispIsStr, isa<StringAtom>(**params));

std::shared_ptr<SExpr>
lispStrLen(std::vector<std::shared_ptr<SExpr>>::iterator params,
           const uint8_t argc) {
  return std::make_shared<IntAtom>(cast<StringAtom>(*params)->literal.size());
}

PRED_OP(lispIsNull, isa<NilAtom>(**params));
PRED_OP(lispIsCons, isa<SExprs>(**params));

std::shared_ptr<SExpr>
lispCons(std::vector<std::shared_ptr<SExpr>>::iterator params,
         const uint8_t argc) {
  return std::make_shared<SExprs>(*params, *(params + 1));
}

std::shared_ptr<SExpr>
lispCar(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  return cast<SExprs>(*params)->first;
}

std::shared_ptr<SExpr>
lispCdr(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  return cast<SExprs>(*params)->rest;
}

std::shared_ptr<SExpr>
lispDisplay(std::vector<std::shared_ptr<SExpr>>::iterator params,
            const uint8_t argc) {
  std::cout << **params << std::endl;
  return std::make_shared<NilAtom>();
}

#undef MATH_CMP_OP
#undef MATH_CUM_OP
#undef MATH_DIM_OP
#undef PRED_OP
