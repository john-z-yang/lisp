#ifndef LISP_SRC_COMPILE_COMPILER_HPP_
#define LISP_SRC_COMPILE_COMPILER_HPP_

#include "../code/Code.hpp"
#include "../sexpr/FunctionAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "SourceLoc.hpp"
#include <memory>
#include <unordered_map>

class Compiler {
  typedef std::function<void(std::shared_ptr<SExpr>)> Visitor;

  struct Local {
    std::shared_ptr<SymAtom> symbol;
    unsigned int depth;
  };

  SourceLoc sourceLoc;
  const std::shared_ptr<SExpr> argNames;
  const std::shared_ptr<SExpr> body;
  std::shared_ptr<FunctionAtom> function;
  std::vector<Local> locals;
  unsigned int scopeDepth;

  void compile(std::shared_ptr<SExpr> sExpr);
  void compileSym(std::shared_ptr<SymAtom> sym);
  void compileDef(std::shared_ptr<SExpr> sExpr);
  void compileSet(std::shared_ptr<SExpr> sExpr);
  void compileIf(std::shared_ptr<SExpr> sExpr);
  void compileLambda(std::shared_ptr<SExpr> sExpr);
  const unsigned int visitEach(std::shared_ptr<SExpr> sExpr, Visitor visitor);
  std::shared_ptr<SExpr> at(const unsigned int n, std::shared_ptr<SExpr> sExpr);
  void beginScope();
  void endScope();
  void setScope(unsigned int scope);
  std::vector<Local>::reverse_iterator findLocal(std::shared_ptr<SymAtom> sym);
  Code &getCode();
  void handleSyntaxError(std::string expected, std::shared_ptr<SExpr> actual);

public:
  Compiler(std::vector<std::string> lines);
  Compiler(std::shared_ptr<SExpr> argNames, std::shared_ptr<SExpr> body,
           unsigned int scopeDepth, SourceLoc sourceLoc);

  std::shared_ptr<FunctionAtom> compile();
};

#endif