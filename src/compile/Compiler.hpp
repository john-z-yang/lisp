#ifndef LISP_SRC_COMPILE_COMPILER_HPP_
#define LISP_SRC_COMPILE_COMPILER_HPP_

#include "../code/Code.hpp"
#include "../sexpr/FnAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/SymAtom.hpp"
#include "SourceLoc.hpp"
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

class Compiler {
public:
  Compiler(std::vector<std::string> source);

  std::shared_ptr<FnAtom> compile();

private:
  typedef std::function<void(std::shared_ptr<SExpr>)> Visitor;

  struct Local {
    const std::shared_ptr<SymAtom> symbol;
    const uint8_t stackOffset;
  };

  struct UpValue {
    int idx;
    bool isLocal;
  };

  Compiler(const std::vector<std::string> source, SourceLoc sourceLoc,
           std::shared_ptr<SExpr> arg, std::shared_ptr<SExpr> body,
           Compiler *enclosing);

  std::vector<std::string> source;
  SourceLoc sourceLoc;

  Compiler *const enclosing;
  const std::shared_ptr<SExpr> arg;
  const std::shared_ptr<SExpr> body;
  std::shared_ptr<FnAtom> function;
  std::vector<Local> locals;
  std::vector<UpValue> upValues;
  uint8_t stackOffset;

  void compile(std::shared_ptr<SExpr> sExpr);
  void compileSym(std::shared_ptr<SymAtom> sym);
  void compileQuote(std::shared_ptr<SExpr> sExpr);
  void compileDef(std::shared_ptr<SExpr> sExpr);
  void compileSet(std::shared_ptr<SExpr> sExpr);
  void compileIf(std::shared_ptr<SExpr> sExpr);
  void compileLambda(std::shared_ptr<SExpr> sExpr);
  void compileCall(std::shared_ptr<SExprs> sExprs);

  void handleSyntaxError(const std::string grammar, const std::string expected,
                         const std::shared_ptr<SExpr> actual);

  const unsigned int visitEach(std::shared_ptr<SExpr> sExpr, Visitor visitor);
  std::shared_ptr<SExpr> at(const unsigned int n, std::shared_ptr<SExpr> sExpr);
  Code &getCode();

  int resolveLocal(std::shared_ptr<SymAtom> sym);
  int resolveUpvalue(Compiler &caller, std::shared_ptr<SymAtom> sym);
  int addUpvalue(int idx, bool isLocal);
};

#endif
