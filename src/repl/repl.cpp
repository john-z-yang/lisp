#include "../../include/repl/repl.hpp"
#include "../../include/env/Env.hpp"
#include "../../include/env/functions.hpp"
#include "../../include/sexpr/Atom.hpp"
#include "../../include/sexpr/BoolAtom.hpp"
#include "../../include/sexpr/ClosureAtom.hpp"
#include "../../include/sexpr/IntAtom.hpp"
#include "../../include/sexpr/SExpr.hpp"
#include "../../include/sexpr/SymAtom.hpp"

using std::all_of;
using std::cin;
using std::cout;
using std::dynamic_pointer_cast;
using std::endl;
using std::getline;
using std::make_pair;
using std::make_shared;
using std::shared_ptr;
using std::stoi;
using std::string;
using std::vector;

vector<string> tokenize(string expression) {
  vector<string> tokens;
  string token;
  for (const char c : expression) {
    if (c == '(' || c == ')' || c == ' ') {
      if (!token.empty()) {
        tokens.push_back(token);
        token = "";
      }
      if (c != ' ') {
        tokens.push_back(string(1, c));
      }
    } else {
      token += string(1, c);
    }
  }
  if (!token.empty()) {
    tokens.push_back(token);
  }
  return tokens;
}

shared_ptr<SExprs> parse(vector<string> tokens) {
  auto it = tokens.begin() + 1;
  return parse(it);
}

shared_ptr<SExprs> parse(vector<string>::iterator &it) {
  string token = *it;
  it += 1;
  if (token == ")") {
    return nullptr;
  } else if (token == "(") {
    shared_ptr<SExpr> first = parse(it);
    return make_shared<SExprs>(first, parse(it));
  } else if (all_of(token.begin(), token.end(), ::isdigit)) {
    shared_ptr<IntAtom> first = make_shared<IntAtom>(stoi(token));
    return make_shared<SExprs>(first, parse(it));
  }
  shared_ptr<SymAtom> first = make_shared<SymAtom>(token);
  return make_shared<SExprs>(first, parse(it));
}

shared_ptr<SExpr> eval(shared_ptr<SExpr> sExpr, shared_ptr<Env> env) {
  if (!sExpr || sExpr->type == SExpr::Type::NUM ||
      sExpr->type == SExpr::Type::BOOL) {
    return sExpr;
  } else if (sExpr->type == SExpr::Type::SYM) {
    return env->find(dynamic_pointer_cast<SymAtom>(sExpr)->val);
  }
  shared_ptr<SExprs> sExprs = dynamic_pointer_cast<SExprs>(sExpr);
  shared_ptr<SymAtom> sym = dynamic_pointer_cast<SymAtom>(sExprs->first);
  if (sym && sym->val == "define") {
    sExprs = dynamic_pointer_cast<SExprs>(sExprs->rest);
    string varName = dynamic_pointer_cast<SymAtom>(sExprs->first)->val;
    shared_ptr<SExpr> varVal =
        eval(dynamic_pointer_cast<SExprs>(sExprs->rest)->first, env);
    env->symTable.insert(make_pair(varName, varVal));

    return varVal;
  } else if (sym && sym->val == "set!") {
    sExprs = dynamic_pointer_cast<SExprs>(sExprs->rest);
    string varName = dynamic_pointer_cast<SymAtom>(sExprs->first)->val;
    shared_ptr<SExpr> varVal =
        eval(dynamic_pointer_cast<SExprs>(sExprs->rest)->first, env);
    env->symTable[varName] = varVal;

    return varVal;
  } else if (sym && sym->val == "if") {
    shared_ptr<BoolAtom> test = dynamic_pointer_cast<BoolAtom>(
        eval(dynamic_pointer_cast<SExprs>(sExprs->rest)->first, env));
    shared_ptr<SExpr> conseq =
        dynamic_pointer_cast<SExprs>(
            dynamic_pointer_cast<SExprs>(sExprs->rest)->rest)
            ->first;
    shared_ptr<SExpr> alt =
        dynamic_pointer_cast<SExprs>(
            dynamic_pointer_cast<SExprs>(
                dynamic_pointer_cast<SExprs>(sExprs->rest)->rest)
                ->rest)
            ->first;

    return (test->val) ? eval(conseq, env) : eval(alt, env);
  } else if (sym && sym->val == "lambda") {
    shared_ptr<SExprs> argNames = dynamic_pointer_cast<SExprs>(
        dynamic_pointer_cast<SExprs>(sExprs->rest)->first);
    shared_ptr<SExpr> body = dynamic_pointer_cast<SExpr>(
        dynamic_pointer_cast<SExprs>(
            dynamic_pointer_cast<SExprs>(sExprs->rest)->rest)
            ->first);

    return make_shared<ClosureAtom>(
        [body](shared_ptr<Env> env) { return eval(body, env); }, env, argNames,
        false);
  }
  shared_ptr<ClosureAtom> closure =
      dynamic_pointer_cast<ClosureAtom>(eval(sExprs->first, env));
  return (*closure)(dynamic_pointer_cast<SExprs>(sExprs->rest), env);
}

void repl() {
  shared_ptr<Env> env = make_shared<Env>();
  initEnv(env);

  while (true) {
    cout << "lisp> ";
    string input;
    getline(cin, input);
    cout << *eval(parse(tokenize(input)), env) << endl;
  }
}
