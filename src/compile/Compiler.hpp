#ifndef LISP_SRC_COMPILE_COMPILER_HPP_
#define LISP_SRC_COMPILE_COMPILER_HPP_

#include "../code/Code.hpp"
#include "../sexpr/FnAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SymAtom.hpp"
#include "SourceLoc.hpp"
#include <memory>
#include <unordered_map>
#include <vector>

class Compiler {
public:
  Compiler(std::vector<std::string> lines);

  std::shared_ptr<FnAtom> compile();

private:
  typedef std::function<void(std::shared_ptr<SExpr>)> Visitor;

  struct Local {
    std::shared_ptr<SymAtom> symbol;
    unsigned int depth;
  };

  struct UpValue {
    int idx;
    bool isLocal;
  };

  Compiler(std::shared_ptr<SExpr> arg, std::shared_ptr<SExpr> body,
           unsigned int scopeDepth, SourceLoc sourceLoc, Compiler *enclosing);

  Compiler *const enclosing;
  SourceLoc sourceLoc;
  const std::shared_ptr<SExpr> arg;
  const std::shared_ptr<SExpr> body;
  std::shared_ptr<FnAtom> function;
  std::vector<Local> locals;
  std::vector<UpValue> upValues;
  unsigned int scopeDepth;

  void compile(std::shared_ptr<SExpr> sExpr);
  void compileSym(std::shared_ptr<SymAtom> sym);
  void compileQuote(std::shared_ptr<SExpr> sExpr);
  void compileDef(std::shared_ptr<SExpr> sExpr);
  void compileSet(std::shared_ptr<SExpr> sExpr);
  void compileIf(std::shared_ptr<SExpr> sExpr);
  void compileLambda(std::shared_ptr<SExpr> sExpr);

  void handleSyntaxError(std::string expected, std::shared_ptr<SExpr> actual);

  const unsigned int visitEach(std::shared_ptr<SExpr> sExpr, Visitor visitor);
  std::shared_ptr<SExpr> at(const unsigned int n, std::shared_ptr<SExpr> sExpr);
  Code &getCode();

  int resolveLocal(std::shared_ptr<SymAtom> sym);
  int resolveUpvalue(Compiler &caller, std::shared_ptr<SymAtom> sym);
  int addUpvalue(int idx, bool isLocal);

  void beginScope();
  void endScope();
  void setScope(unsigned int scope);
};

#endif
