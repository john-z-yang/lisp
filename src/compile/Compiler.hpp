#ifndef LISP_SRC_COMPILE_COMPILER_HPP_
#define LISP_SRC_COMPILE_COMPILER_HPP_

#include "../code/Code.hpp"
#include "../sexpr/FunctionAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include <memory>

class Compiler {
  typedef std::function<void(std::shared_ptr<SExpr>)> Visitor;

  struct Local {
    std::shared_ptr<SymAtom> symbol;
    unsigned int depth;
  };

  std::shared_ptr<FunctionAtom> function;
  const std::shared_ptr<SExpr> argNames;
  const std::shared_ptr<SExpr> body;
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
  void handleSyntaxError(std::string expected, std::shared_ptr<SExpr> actual);
  void beginScope();
  void endScope();
  void setScope(unsigned int scope);
  std::vector<Local>::reverse_iterator findLocal(std::shared_ptr<SymAtom> sym);
  Code &getCode();

public:
  Compiler(std::shared_ptr<SExpr> root);
  Compiler(std::shared_ptr<SExpr> argNames, std::shared_ptr<SExpr> body,
           unsigned int scopeDepth);

  std::shared_ptr<FunctionAtom> compile();
};

#endif