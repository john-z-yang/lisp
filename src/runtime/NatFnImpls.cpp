#include "NatFnImpls.hpp"
#include "../common/cast.cpp"
#include "../common/sexpr/BoolAtom.hpp"
#include "../common/sexpr/ClosureAtom.hpp"
#include "../common/sexpr/IntAtom.hpp"
#include "../common/sexpr/NilAtom.hpp"
#include "../common/sexpr/SExprs.hpp"
#include "../common/sexpr/StringAtom.hpp"
#include "../common/sexpr/SymAtom.hpp"
#include <cstdlib>
#include <exception>
#include <memory>
#include <stdexcept>

#define MATH_CMP_OP(name, op)                                                  \
  std::shared_ptr<SExpr> name(                                                 \
      std::vector<std::shared_ptr<SExpr>>::iterator params,                    \
      const uint8_t argc) {                                                    \
    const auto prev = cast<IntAtom>(*params)->val;                             \
    ++params;                                                                  \
    for (uint8_t i{1}; i < argc; ++i) {                                        \
      if (!(prev op cast<IntAtom>(*params)->val)) {                            \
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
    for (uint8_t i{0}; i < argc; ++i) {                                        \
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
    for (uint8_t i{1}; i < argc; ++i) {                                        \
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
MATH_CMP_OP(lispNumEq, ==);
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
  return std::make_shared<IntAtom>(abs(cast<IntAtom>(*params)->val));
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
  return std::make_shared<IntAtom>(cast<StringAtom>(*params)->unescaped.size());
}
std::shared_ptr<SExpr>
lispStrSub(std::vector<std::shared_ptr<SExpr>>::iterator params,
           const uint8_t argc) {
  auto str = cast<StringAtom>(*params);
  auto pos = cast<IntAtom>(*(params + 1))->val;
  auto len = cast<IntAtom>(*(params + 2))->val;
  std::stringstream ss;
  try {
    ss << "\"" << str->unescaped.substr(pos, len) << "\"";
  } catch (std::out_of_range &ofr) {
    std::stringstream ess;
    ess << "Invalid range for " << str->literal << " (" << pos << ", " << len
        << ")";
    throw std::invalid_argument(ess.str());
  }
  return std::make_shared<StringAtom>(ss.str());
}
std::shared_ptr<SExpr>
lispStrCon(std::vector<std::shared_ptr<SExpr>>::iterator params,
           const uint8_t argc) {
  std::stringstream ss;
  ss << "\"";
  for (uint8_t i{0}; i < argc; ++i) {
    ss << cast<StringAtom>(*params)->unescaped;
    ++params;
  }
  ss << "\"";
  return std::make_shared<StringAtom>(ss.str());
}
std::shared_ptr<SExpr>
lispToStr(std::vector<std::shared_ptr<SExpr>>::iterator params,
          const uint8_t argc) {
  if (isa<StringAtom>(**params)) {
    return *params;
  }
  std::stringstream ss;
  ss << "\"";
  for (uint8_t i{0}; i < argc; ++i) {
    ss << **params;
    ++params;
  }
  ss << "\"";
  return std::make_shared<StringAtom>(ss.str());
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
lispDis(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  cast<ClosureAtom>(*params)->dissassemble(std::cout);
  return std::make_shared<NilAtom>();
}
std::shared_ptr<SExpr>
lispDisplay(std::vector<std::shared_ptr<SExpr>>::iterator params,
            const uint8_t argc) {
  if (auto stringAtom = std::dynamic_pointer_cast<StringAtom>(*params)) {
    std::cout << stringAtom->unescaped << std::endl;
  } else {
    std::cout << **params << std::endl;
  }
  return std::make_shared<NilAtom>();
}

std::shared_ptr<SExpr>
lispQuit(std::vector<std::shared_ptr<SExpr>>::iterator params,
         const uint8_t argc) {
  std::cout << "Farewell." << std::endl;
  exit(0);
}
std::shared_ptr<SExpr>
lispError(std::vector<std::shared_ptr<SExpr>>::iterator params,
          const uint8_t argc) {
  std::stringstream ss;
  ss << **params;
  throw std::runtime_error(ss.str());
}

std::shared_ptr<SExpr>
lispEq(std::vector<std::shared_ptr<SExpr>>::iterator params,
       const uint8_t argc) {
  return std::make_shared<BoolAtom>((*params) == (*(params + 1)));
}
std::shared_ptr<SExpr>
lispEqv(std::vector<std::shared_ptr<SExpr>>::iterator params,
        const uint8_t argc) {
  return std::make_shared<BoolAtom>(**params == **(params + 1));
}

#undef MATH_CMP_OP
#undef MATH_CUM_OP
#undef MATH_DIM_OP
#undef PRED_OP
