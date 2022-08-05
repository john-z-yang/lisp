#include "../../include/repl/repl.hpp"
#include "../../include/env/Env.hpp"
#include "../../include/env/functions.hpp"
#include "../../include/sexpr/Atom.hpp"
#include "../../include/sexpr/BoolAtom.hpp"
#include "../../include/sexpr/ClosureAtom.hpp"
#include "../../include/sexpr/IntAtom.hpp"
#include "../../include/sexpr/SExpr.hpp"
#include "../../include/sexpr/SymAtom.hpp"

std::vector<std::string> tokenize(std::string expression) {
  std::vector<std::string> tokens;
  std::string token;
  for (const char c : expression) {
    if (c == '(' || c == ')' || c == ' ') {
      if (!token.empty()) {
        tokens.push_back(token);
        token = "";
      }
      if (c != ' ') {
        tokens.push_back(std::string(1, c));
      }
    } else {
      token += std::string(1, c);
    }
  }
  if (!token.empty()) {
    tokens.push_back(token);
  }
  return tokens;
}

std::shared_ptr<SExprs> parse(std::vector<std::string> tokens) {
  auto it = tokens.begin() + 1;
  return parse(it);
}

std::shared_ptr<SExprs> parse(std::vector<std::string>::iterator &it) {
  std::string token = *it;
  it += 1;
  if (token == ")") {
    return nullptr;
  } else if (token == "(") {
    std::shared_ptr<SExpr> first = parse(it);
    return std::make_shared<SExprs>(first, parse(it));
  } else if (std::all_of(token.begin(), token.end(), ::isdigit)) {
    std::shared_ptr<IntAtom> first =
        std::make_shared<IntAtom>(std::stoi(token));
    return std::make_shared<SExprs>(first, parse(it));
  }
  std::shared_ptr<SymAtom> first = std::make_shared<SymAtom>(token);
  return std::make_shared<SExprs>(first, parse(it));
}

std::shared_ptr<SExpr> eval(std::shared_ptr<SExpr> sExpr,
                            std::shared_ptr<Env> env) {
  if (!sExpr || sExpr->type == SExpr::Type::NUM ||
      sExpr->type == SExpr::Type::BOOL) {
    return sExpr;
  } else if (sExpr->type == SExpr::Type::SYM) {
    return env->find(std::dynamic_pointer_cast<SymAtom>(sExpr)->val);
  }
  std::shared_ptr<SExprs> sExprs = std::dynamic_pointer_cast<SExprs>(sExpr);
  std::shared_ptr<SymAtom> sym =
      std::dynamic_pointer_cast<SymAtom>(sExprs->first);
  if (sym && sym->val == "define") {
    sExprs = std::dynamic_pointer_cast<SExprs>(sExprs->rest);
    std::string varName =
        std::dynamic_pointer_cast<SymAtom>(sExprs->first)->val;
    std::shared_ptr<SExpr> varVal =
        eval(std::dynamic_pointer_cast<SExprs>(sExprs->rest)->first, env);
    env->symTable.insert(std::make_pair(varName, varVal));

    return varVal;
  } else if (sym && sym->val == "set!") {
    sExprs = std::dynamic_pointer_cast<SExprs>(sExprs->rest);
    std::string varName =
        std::dynamic_pointer_cast<SymAtom>(sExprs->first)->val;
    std::shared_ptr<SExpr> varVal =
        eval(std::dynamic_pointer_cast<SExprs>(sExprs->rest)->first, env);
    env->symTable[varName] = varVal;

    return varVal;
  } else if (sym && sym->val == "if") {
    std::shared_ptr<BoolAtom> test = std::dynamic_pointer_cast<BoolAtom>(
        eval(std::dynamic_pointer_cast<SExprs>(sExprs->rest)->first, env));
    std::shared_ptr<SExpr> conseq =
        std::dynamic_pointer_cast<SExprs>(
            std::dynamic_pointer_cast<SExprs>(sExprs->rest)->rest)
            ->first;
    std::shared_ptr<SExpr> alt =
        std::dynamic_pointer_cast<SExprs>(
            std::dynamic_pointer_cast<SExprs>(
                std::dynamic_pointer_cast<SExprs>(sExprs->rest)->rest)
                ->rest)
            ->first;

    return (test->val) ? eval(conseq, env) : eval(alt, env);
  } else if (sym && sym->val == "lambda") {
    std::shared_ptr<SExprs> argNames = std::dynamic_pointer_cast<SExprs>(
        std::dynamic_pointer_cast<SExprs>(sExprs->rest)->first);
    std::shared_ptr<SExpr> body = std::dynamic_pointer_cast<SExpr>(
        std::dynamic_pointer_cast<SExprs>(
            std::dynamic_pointer_cast<SExprs>(sExprs->rest)->rest)
            ->first);

    return std::make_shared<ClosureAtom>(
        [body](std::shared_ptr<Env> env) { return eval(body, env); }, env,
        argNames, false);
  }
  std::shared_ptr<ClosureAtom> closure =
      std::dynamic_pointer_cast<ClosureAtom>(eval(sExprs->first, env));
  return (*closure)(std::dynamic_pointer_cast<SExprs>(sExprs->rest), env);
}

void repl() {
  std::shared_ptr<Env> env = std::make_shared<Env>();
  initEnv(env);

  while (true) {
    std::cout << "lisp> ";
    std::string input;
    std::getline(std::cin, input);
    std::cout << *eval(parse(tokenize(input)), env) << std::endl;
  }
}
