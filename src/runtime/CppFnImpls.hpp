#ifndef LISP_SRC_RUNTIME_NATFNIMPLS_HPP_
#define LISP_SRC_RUNTIME_NATFNIMPLS_HPP_

#include "CppFn.hpp"
#include <memory>
#include <vector>

namespace runtime {

CppFn lispIsSym;
CppFn lispGenSym;

CppFn lispIsNum;
CppFn lispNumEq;
CppFn lispGt;
CppFn lispGteq;
CppFn lispLt;
CppFn lispLteq;
CppFn lispAdd;
CppFn lispSub;
CppFn lispMult;
CppFn lispDiv;
CppFn lispAbs;
CppFn lispMod;

CppFn lispIsStr;
CppFn lispStrLen;
CppFn lispStrSub;
CppFn lispStrCon;
CppFn lispToStr;

CppFn lispIsNull;
CppFn lispIsCons;
CppFn lispCons;
CppFn lispCar;
CppFn lispCdr;

CppFn lispDis;
CppFn lispIsClosure;
CppFn lispDisplay;

CppFn lispQuit;
CppFn lispError;

CppFn lispEq;
CppFn lispEqv;

CppFn lispIsProc;

} // namespace runtime

#endif
