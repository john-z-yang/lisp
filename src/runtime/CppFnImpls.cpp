#include "CppFnImpls.hpp"
#include "../sexpr/Bool.hpp"
#include "../sexpr/Closure.hpp"
#include "../sexpr/NatFn.hpp"
#include "../sexpr/Nil.hpp"
#include "../sexpr/Num.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/String.hpp"
#include "../sexpr/Sym.hpp"
#include "../sexpr/cast.cpp"
#include <cmath>
#include <cstdlib>
#include <exception>
#include <memory>
#include <stdexcept>

using namespace sexpr;
using namespace runtime;

#define MATH_CMP_OP(name, op)                                                  \
  const SExpr *name(StackIter params, const uint8_t argc, VM &vm) {            \
    const auto prev = cast<Num>(*params)->val;                                 \
    ++params;                                                                  \
    for (uint8_t i{1}; i < argc; ++i) {                                        \
      if (!(prev op cast<Num>(*params)->val)) {                                \
        return vm.alloc<Bool>(false);                                          \
      }                                                                        \
      ++params;                                                                \
    }                                                                          \
    return vm.alloc<Bool>(true);                                               \
  }

#define MATH_CUM_OP(name, op, init)                                            \
  const SExpr *name(StackIter params, const uint8_t argc, VM &vm) {            \
    auto res = init;                                                           \
    for (uint8_t i{0}; i < argc; ++i) {                                        \
      res op cast<Num>(*params)->val;                                          \
      ++params;                                                                \
    }                                                                          \
    return vm.alloc<Num>(res);                                                 \
  }

#define MATH_DIM_OP(name, op, unaryOp)                                         \
  const SExpr *name(StackIter params, const uint8_t argc, VM &vm) {            \
    if (argc == 1) {                                                           \
      return vm.alloc<Num>(unaryOp cast<Num>(*params)->val);                   \
    }                                                                          \
    auto res = cast<Num>(*params)->val;                                        \
    ++params;                                                                  \
    for (uint8_t i{1}; i < argc; ++i) {                                        \
      res op cast<Num>(*params)->val;                                          \
      ++params;                                                                \
    }                                                                          \
    return vm.alloc<Num>(res);                                                 \
  }

#define PRED_OP(name, cond)                                                    \
  const SExpr *name(StackIter params, const uint8_t argc, VM &vm) {            \
    return vm.alloc<Bool>(cond);                                               \
  }

PRED_OP(runtime::lispIsSym, isa<Sym>(*params));

long genSymCnt = 0;
const SExpr *runtime::lispGenSym(StackIter params, const uint8_t argc, VM &vm) {
  std::stringstream ss;
  ss << ";gensym-" << genSymCnt;
  genSymCnt += 1;
  return vm.alloc<Sym>(ss.str());
}

PRED_OP(runtime::lispIsNum, isa<Num>(*params));
MATH_CMP_OP(runtime::lispNumEq, ==);
MATH_CMP_OP(runtime::lispGt, >);
MATH_CMP_OP(runtime::lispGteq, >=);
MATH_CMP_OP(runtime::lispLt, <);
MATH_CMP_OP(runtime::lispLteq, <=);
MATH_CUM_OP(runtime::lispAdd, +=, 0.0);
MATH_CUM_OP(runtime::lispMult, *=, 1.0);
MATH_DIM_OP(runtime::lispSub, -=, 0.0 -);
MATH_DIM_OP(runtime::lispDiv, /=, 1.0 /);
const SExpr *runtime::lispAbs(StackIter params, const uint8_t argc, VM &vm) {
  return vm.alloc<Num>(abs(cast<Num>(*params)->val));
}
const SExpr *runtime::lispMod(StackIter params, const uint8_t argc, VM &vm) {
  const auto lhs = cast<Num>(*params)->val;
  ++params;
  const auto rhs = cast<Num>(*params)->val;
  return vm.alloc<Num>(std::fmod(lhs, rhs));
}

PRED_OP(runtime::lispIsStr, isa<String>(*params));
const SExpr *runtime::lispStrLen(StackIter params, const uint8_t argc, VM &vm) {
  return vm.alloc<Num>(cast<String>(*params)->unescaped.size());
}
const SExpr *runtime::lispStrSub(StackIter params, const uint8_t argc, VM &vm) {
  auto str = cast<String>(*params);
  auto pos = cast<Num>(*(params + 1))->val;
  auto len = cast<Num>(*(params + 2))->val;
  std::stringstream ss;
  try {
    ss << "\"" << str->unescaped.substr(pos, len) << "\"";
  } catch (std::out_of_range &ofr) {
    std::stringstream ess;
    ess << "Invalid range for " << str->literal << " (" << pos << ", " << len
        << ")";
    throw std::invalid_argument(ess.str());
  }
  return vm.alloc<String>(ss.str());
}
const SExpr *runtime::lispStrCon(StackIter params, const uint8_t argc, VM &vm) {
  std::stringstream ss;
  ss << "\"";
  for (uint8_t i{0}; i < argc; ++i) {
    ss << cast<String>(*params)->unescaped;
    ++params;
  }
  ss << "\"";
  return vm.alloc<String>(ss.str());
}
const SExpr *runtime::lispToStr(StackIter params, const uint8_t argc, VM &vm) {
  if (isa<String>(*params)) {
    return *params;
  }
  std::stringstream ss;
  ss << "\"";
  for (uint8_t i{0}; i < argc; ++i) {
    ss << **params;
    ++params;
  }
  ss << "\"";
  return vm.alloc<String>(ss.str());
}

PRED_OP(runtime::lispIsNull, isa<Nil>(*params));
PRED_OP(runtime::lispIsCons, isa<SExprs>(*params));
const SExpr *runtime::lispCons(StackIter params, const uint8_t argc, VM &vm) {
  return vm.alloc<SExprs>(*params, *(params + 1));
}
const SExpr *runtime::lispCar(StackIter params, const uint8_t argc, VM &vm) {
  return cast<SExprs>(*params)->first;
}
const SExpr *runtime::lispCdr(StackIter params, const uint8_t argc, VM &vm) {
  return cast<SExprs>(*params)->rest;
}

const SExpr *runtime::lispDis(StackIter params, const uint8_t argc, VM &vm) {
  cast<Closure>(*params)->dissassemble(std::cout);
  return vm.alloc<Nil>();
}
const SExpr *runtime::lispDisplay(StackIter params, const uint8_t argc,
                                  VM &vm) {
  if (auto stringAtom = dynCast<String>(*params)) {
    std::cout << stringAtom->unescaped << std::endl;
  } else {
    std::cout << **params << std::endl;
  }
  return vm.alloc<Nil>();
}

const SExpr *runtime::lispQuit(StackIter params, const uint8_t argc, VM &vm) {
  std::cout << "Farewell." << std::endl;
  exit(0);
}
const SExpr *runtime::lispError(StackIter params, const uint8_t argc, VM &vm) {
  std::stringstream ss;
  ss << **params;
  throw std::runtime_error(ss.str());
}

const SExpr *runtime::lispEq(StackIter params, const uint8_t argc, VM &vm) {
  return vm.alloc<Bool>((*params) == (*(params + 1)));
}
const SExpr *runtime::lispEqv(StackIter params, const uint8_t argc, VM &vm) {
  return vm.alloc<Bool>(**params == **(params + 1));
}

const SExpr *runtime::lispIsProc(StackIter params, const uint8_t argc, VM &vm) {
  return vm.alloc<Bool>(isa<Closure>(*params) || isa<NatFn>(*params));
}

#undef MATH_CMP_OP
#undef MATH_CUM_OP
#undef MATH_DIM_OP
#undef PRED_OP
