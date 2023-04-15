#include "NatFnImpls.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/cast.cpp"

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
lispDisplay(std::vector<std::shared_ptr<SExpr>>::iterator params,
            const uint8_t argc) {
  std::cout << **params << std::endl;
  return std::make_shared<NilAtom>();
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

#undef MATH_CMP_OP
#undef MATH_CUM_OP
#undef MATH_DIM_OP
