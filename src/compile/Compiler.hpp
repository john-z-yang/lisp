#ifndef LISP_SRC_COMPILE_COMPILER_HPP_
#define LISP_SRC_COMPILE_COMPILER_HPP_

#include "../common/Code.hpp"
#include "../common/sexpr/FnAtom.hpp"
#include "../common/sexpr/SExpr.hpp"
#include "../common/sexpr/SExprs.hpp"
#include "../common/sexpr/SymAtom.hpp"
#include "../runtime/VM.hpp"
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

class Compiler {
private:
  typedef std::unordered_map<SExpr *,
                             std::tuple<const unsigned int, const unsigned int>>
      SourceLoc;

  struct Token {
    unsigned int row;
    unsigned int col;
    std::string str;
  };

  typedef std::function<void(SExpr *)> Visitor;

  struct Local {
    SymAtom *const symbol;
    const uint8_t stackOffset;
    bool isCaptured;
  };

  struct UpValue {
    int idx;
    bool isLocal;
  };

  std::vector<std::string> source;
  SourceLoc sourceLoc;

  VM &vm;

  Compiler *const enclosing;
  SExpr *const arg;
  SExpr *const body;
  FnAtom *function;
  std::vector<Local> locals;
  std::vector<UpValue> upValues;
  uint8_t stackOffset;

  Compiler(const std::vector<std::string> source, SourceLoc sourceLoc,
           SExpr *arg, SExpr *body, Compiler *enclosing, VM &vm);

  static std::vector<Token> tokenize(std::vector<std::string> lines);
  static std::vector<Token> tokenize(std::string line, const unsigned int row);

  SExpr *parse(std::vector<std::string> lines, SourceLoc &sourceLoc);
  SExpr *parse(std::vector<Token>::const_iterator &it, SourceLoc &sourceLoc);
  SExpr *parseAtom(Token token);
  SExpr *parseSexprs(std::vector<Token>::const_iterator &it,
                     SourceLoc &sourceLoc);

  void compile(SExpr *sExpr);
  void compileSym(SymAtom *sym);
  void compileQuote(SExpr *sExpr);
  void compileDef(SExpr *sExpr);
  void compileDefMacro(SExpr *sExpr);
  void compileSet(SExpr *sExpr);
  void compileIf(SExpr *sExpr);
  void compileLambda(SExpr *sExpr);
  void compileCall(SExprs *sExprs);

  SExpr *expandMacro(SExpr *macro);

  unsigned int visitEach(SExpr *sExpr, Visitor visitor);
  SExpr *at(const unsigned int n, SExpr *sExpr);
  Code &getCode();

  int resolveLocal(SymAtom *sym);
  int resolveUpvalue(Compiler &caller, SymAtom *sym);
  int addUpvalue(int idx, bool isLocal);

  static void handleUnexpectedToken(const Token &token,
                                    const std::string &line);
  void handleSyntaxError(const std::string grammar, const std::string expected,
                         SExpr *const actual);

public:
  Compiler(std::vector<std::string> source, VM &vm);

  FnAtom *compile();

  static void verifyLex(std::string &line, const unsigned int lineNum,
                        uint32_t &openParen, uint32_t &closedParen);
};

#endif
