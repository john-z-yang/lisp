#include "NatFnImpls.hpp"
#include "../common/cast.cpp"
#include "../common/sexpr/BoolAtom.hpp"
#include "../common/sexpr/ClosureAtom.hpp"
#include "../common/sexpr/IntAtom.hpp"
#include "../common/sexpr/NatFnAtom.hpp"
#include "../common/sexpr/NilAtom.hpp"
#include "../common/sexpr/SExprs.hpp"
#include "../common/sexpr/StringAtom.hpp"
#include "../common/sexpr/SymAtom.hpp"
#include <cstdlib>
#include <exception>
#include <memory>
#include <stdexcept>

#define MATH_CMP_OP(name, op)                                                  \
  SExpr *name(std::vector<SExpr *>::iterator params, const uint8_t argc,       \
              VM &vm) {                                                        \
    const auto prev = cast<IntAtom>(*params)->val;                             \
    ++params;                                                                  \
    for (uint8_t i{1}; i < argc; ++i) {                                        \
      if (!(prev op cast<IntAtom>(*params)->val)) {                            \
        return vm.alloc<BoolAtom>(false);                                      \
      }                                                                        \
      ++params;                                                                \
    }                                                                          \
    return vm.alloc<BoolAtom>(true);                                           \
  }

#define MATH_CUM_OP(name, op, init)                                            \
  SExpr *name(std::vector<SExpr *>::iterator params, const uint8_t argc,       \
              VM &vm) {                                                        \
    int res = init;                                                            \
    for (uint8_t i{0}; i < argc; ++i) {                                        \
      res op cast<IntAtom>(*params)->val;                                      \
      ++params;                                                                \
    }                                                                          \
    return vm.alloc<IntAtom>(res);                                             \
  }

#define MATH_DIM_OP(name, op, unaryOp)                                         \
  SExpr *name(std::vector<SExpr *>::iterator params, const uint8_t argc,       \
              VM &vm) {                                                        \
    if (argc == 1) {                                                           \
      return vm.alloc<IntAtom>(unaryOp cast<IntAtom>(*params)->val);           \
    }                                                                          \
    int res = cast<IntAtom>(*params)->val;                                     \
    ++params;                                                                  \
    for (uint8_t i{1}; i < argc; ++i) {                                        \
      res op cast<IntAtom>(*params)->val;                                      \
      ++params;                                                                \
    }                                                                          \
    return vm.alloc<IntAtom>(res);                                             \
  }

#define PRED_OP(name, cond)                                                    \
  SExpr *name(std::vector<SExpr *>::iterator params, const uint8_t argc,       \
              VM &vm) {                                                        \
    return vm.alloc<BoolAtom>(cond);                                           \
  }

PRED_OP(lispIsSym, isa<SymAtom>(**params));

long genSymCnt = 0;
SExpr *lispGenSym(std::vector<SExpr *>::iterator params, const uint8_t argc,
                  VM &vm) {
  std::stringstream ss;
  ss << ";gensym-" << genSymCnt;
  genSymCnt += 1;
  return vm.alloc<SymAtom>(ss.str());
}

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
SExpr *lispAbs(std::vector<SExpr *>::iterator params, const uint8_t argc,
               VM &vm) {
  return vm.alloc<IntAtom>(abs(cast<IntAtom>(*params)->val));
}
SExpr *lispMod(std::vector<SExpr *>::iterator params, const uint8_t argc,
               VM &vm) {
  const auto lhs = cast<IntAtom>(*params)->val;
  ++params;
  const auto rhs = cast<IntAtom>(*params)->val;
  return vm.alloc<IntAtom>(lhs % rhs);
}

PRED_OP(lispIsStr, isa<StringAtom>(**params));
SExpr *lispStrLen(std::vector<SExpr *>::iterator params, const uint8_t argc,
                  VM &vm) {
  return vm.alloc<IntAtom>(cast<StringAtom>(*params)->unescaped.size());
}
SExpr *lispStrSub(std::vector<SExpr *>::iterator params, const uint8_t argc,
                  VM &vm) {
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
  return vm.alloc<StringAtom>(ss.str());
}
SExpr *lispStrCon(std::vector<SExpr *>::iterator params, const uint8_t argc,
                  VM &vm) {
  std::stringstream ss;
  ss << "\"";
  for (uint8_t i{0}; i < argc; ++i) {
    ss << cast<StringAtom>(*params)->unescaped;
    ++params;
  }
  ss << "\"";
  return vm.alloc<StringAtom>(ss.str());
}
SExpr *lispToStr(std::vector<SExpr *>::iterator params, const uint8_t argc,
                 VM &vm) {
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
  return vm.alloc<StringAtom>(ss.str());
}

PRED_OP(lispIsNull, isa<NilAtom>(**params));
PRED_OP(lispIsCons, isa<SExprs>(**params));
SExpr *lispCons(std::vector<SExpr *>::iterator params, const uint8_t argc,
                VM &vm) {
  return vm.alloc<SExprs>(*params, *(params + 1));
}
SExpr *lispCar(std::vector<SExpr *>::iterator params, const uint8_t argc,
               VM &vm) {
  return cast<SExprs>(*params)->first;
}
SExpr *lispCdr(std::vector<SExpr *>::iterator params, const uint8_t argc,
               VM &vm) {
  return cast<SExprs>(*params)->rest;
}

SExpr *lispDis(std::vector<SExpr *>::iterator params, const uint8_t argc,
               VM &vm) {
  cast<ClosureAtom>(*params)->dissassemble(std::cout);
  return vm.alloc<NilAtom>();
}
SExpr *lispDisplay(std::vector<SExpr *>::iterator params, const uint8_t argc,
                   VM &vm) {
  if (auto stringAtom = dynamic_cast<StringAtom *>(*params)) {
    std::cout << stringAtom->unescaped << std::endl;
  } else {
    std::cout << **params << std::endl;
  }
  return vm.alloc<NilAtom>();
}

SExpr *lispQuit(std::vector<SExpr *>::iterator params, const uint8_t argc,
                VM &vm) {
  std::cout << "Farewell." << std::endl;
  exit(0);
}
SExpr *lispError(std::vector<SExpr *>::iterator params, const uint8_t argc,
                 VM &vm) {
  std::stringstream ss;
  ss << **params;
  throw std::runtime_error(ss.str());
}

SExpr *lispEq(std::vector<SExpr *>::iterator params, const uint8_t argc,
              VM &vm) {
  return vm.alloc<BoolAtom>((*params) == (*(params + 1)));
}
SExpr *lispEqv(std::vector<SExpr *>::iterator params, const uint8_t argc,
               VM &vm) {
  return vm.alloc<BoolAtom>(**params == **(params + 1));
}

SExpr *lispIsProc(std::vector<SExpr *>::iterator params, const uint8_t argc,
                  VM &vm) {
  return vm.alloc<BoolAtom>(isa<ClosureAtom>(**params) ||
                            isa<NatFnAtom>(**params));
}

#undef MATH_CMP_OP
#undef MATH_CUM_OP
#undef MATH_DIM_OP
#undef PRED_OP
