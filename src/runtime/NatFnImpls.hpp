#ifndef LISP_SRC_VM_NATIVEFNS_HPP_
#define LISP_SRC_VM_NATIVEFNS_HPP_

#include "../common/sexpr/SExpr.hpp"
#include "../runtime/VM.hpp"
#include <memory>
#include <vector>

typedef SExpr *(NativeFn)(std::vector<SExpr *>::iterator params,
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

#endif
