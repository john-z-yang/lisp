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
  typedef std::unordered_map<std::shared_ptr<SExpr>,
                             std::tuple<const unsigned int, const unsigned int>>
      SourceLoc;

  struct Token {
    unsigned int row;
    unsigned int col;
    std::string str;
  };

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

  VM &vm;

  Compiler *const enclosing;
  const std::shared_ptr<SExpr> arg;
  const std::shared_ptr<SExpr> body;
  std::shared_ptr<FnAtom> function;
  std::vector<Local> locals;
  std::vector<UpValue> upValues;
  uint8_t stackOffset;

  Compiler(const std::vector<std::string> source, SourceLoc sourceLoc,
           std::shared_ptr<SExpr> arg, std::shared_ptr<SExpr> body,
           Compiler *enclosing, VM &vm);

  static std::vector<Token> tokenize(std::vector<std::string> lines);
  static std::vector<Token> tokenize(std::string line, const unsigned int row);

  static std::shared_ptr<SExpr> parse(std::vector<std::string> lines,
                                      SourceLoc &sourceLoc);
  static std::shared_ptr<SExpr> parse(std::vector<Token>::const_iterator &it,
                                      SourceLoc &sourceLoc);
  static std::shared_ptr<SExpr> parseAtom(Token token);
  static std::shared_ptr<SExpr>
  parseSexprs(std::vector<Token>::const_iterator &it, SourceLoc &sourceLoc);

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

  const unsigned int visitEach(std::shared_ptr<SExpr> sExpr, Visitor visitor);
  std::shared_ptr<SExpr> at(const unsigned int n, std::shared_ptr<SExpr> sExpr);
  Code &getCode();

  int resolveLocal(std::shared_ptr<SymAtom> sym);
  int resolveUpvalue(Compiler &caller, std::shared_ptr<SymAtom> sym);
  int addUpvalue(int idx, bool isLocal);

  static void handleUnexpectedToken(const Token &token,
                                    const std::string &line);
  void handleSyntaxError(const std::string grammar, const std::string expected,
                         const std::shared_ptr<SExpr> actual);

public:
  Compiler(std::vector<std::string> source, VM &vm);

  std::shared_ptr<FnAtom> compile();

  static void verifyLex(std::string &line, const unsigned int lineNum,
                        uint32_t &openParen, uint32_t &closedParen);
};

#endif
