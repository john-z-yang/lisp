#ifndef LISP_SRC_COMPILE_COMPILER_HPP_
#define LISP_SRC_COMPILE_COMPILER_HPP_

#include "../common/Code.hpp"
#include "../common/sexpr/FnAtom.hpp"
#include "../common/sexpr/SExpr.hpp"
#include "../common/sexpr/SExprs.hpp"
#include "../common/sexpr/SymAtom.hpp"
#include "../runtime/VM.hpp"
#include "SourceLoc.hpp"
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

class Compiler {
private:
  typedef std::function<void(std::shared_ptr<SExpr>)> Visitor;

  struct Local {
    const std::shared_ptr<SymAtom> symbol;
    const uint8_t stackOffset;
    bool isCaptured;
  };

  struct UpValue {
    int idx;
    bool isLocal;
  };

  std::vector<std::string> source;
  SourceLoc sourceLoc;

  Compiler *const enclosing;
  const std::shared_ptr<SExpr> arg;
  const std::shared_ptr<SExpr> body;
  std::shared_ptr<FnAtom> function;
  std::vector<Local> locals;
  std::vector<UpValue> upValues;
  uint8_t stackOffset;

  VM &vm;

  Compiler(const std::vector<std::string> source, SourceLoc sourceLoc,
           std::shared_ptr<SExpr> arg, std::shared_ptr<SExpr> body,
           Compiler *enclosing, VM &vm);

  void compile(std::shared_ptr<SExpr> sExpr);
  void compileSym(std::shared_ptr<SymAtom> sym);
  void compileQuote(std::shared_ptr<SExpr> sExpr);
  void compileDef(std::shared_ptr<SExpr> sExpr);
  void compileDefMacro(std::shared_ptr<SExpr> sExpr);
  void compileSet(std::shared_ptr<SExpr> sExpr);
  void compileIf(std::shared_ptr<SExpr> sExpr);
  void compileLambda(std::shared_ptr<SExpr> sExpr);
  void compileCall(std::shared_ptr<SExprs> sExprs);

  std::shared_ptr<SExpr> expandMacro(std::shared_ptr<SExpr>);

  void handleSyntaxError(const std::string grammar, const std::string expected,
                         const std::shared_ptr<SExpr> actual);

  const unsigned int visitEach(std::shared_ptr<SExpr> sExpr, Visitor visitor);
  std::shared_ptr<SExpr> at(const unsigned int n, std::shared_ptr<SExpr> sExpr);
  Code &getCode();

  int resolveLocal(std::shared_ptr<SymAtom> sym);
  int resolveUpvalue(Compiler &caller, std::shared_ptr<SymAtom> sym);
  int addUpvalue(int idx, bool isLocal);

public:
  Compiler(std::vector<std::string> source, VM &vm);

  std::shared_ptr<FnAtom> compile();
};

#endif
