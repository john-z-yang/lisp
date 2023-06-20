#ifndef LISP_SRC_RUNTIME_NATFNIMPLS_HPP_
#define LISP_SRC_RUNTIME_NATFNIMPLS_HPP_

#include "../runtime/VM.hpp"
#include "../sexpr/SExpr.hpp"
#include <memory>
#include <vector>

namespace runtime {

typedef const sexpr::SExpr *(
    NativeFn)(std::vector<const sexpr::SExpr *>::iterator params,
              const uint8_t argc, VM &vm);

NativeFn lispIsSym;
NativeFn lispGenSym;

NativeFn lispIsNum;
NativeFn lispNumEq;
NativeFn lispGt;
NativeFn lispGteq;
NativeFn lispLt;
NativeFn lispLteq;
NativeFn lispAdd;
NativeFn lispSub;
NativeFn lispMult;
NativeFn lispDiv;
NativeFn lispAbs;
NativeFn lispMod;

NativeFn lispIsStr;
NativeFn lispStrLen;
NativeFn lispStrSub;
NativeFn lispStrCon;
NativeFn lispToStr;

NativeFn lispIsNull;
NativeFn lispIsCons;
NativeFn lispCons;
NativeFn lispCar;
NativeFn lispCdr;

NativeFn lispDis;
NativeFn lispIsClosure;
NativeFn lispDisplay;

NativeFn lispQuit;
NativeFn lispError;

NativeFn lispEq;
NativeFn lispEqv;

NativeFn lispIsProc;

} // namespace runtime

#endif
