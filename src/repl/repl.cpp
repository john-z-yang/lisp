#include "../../include/repl/repl.hpp"
#include "../../include/env/Env.hpp"
#include "../../include/env/functions.hpp"
#include "../../include/repl/except/EvalException.hpp"
#include "../../include/sexpr/Atom.hpp"
#include "../../include/sexpr/BoolAtom.hpp"
#include "../../include/sexpr/ClosureAtom.hpp"
#include "../../include/sexpr/IntAtom.hpp"
#include "../../include/sexpr/NilAtom.hpp"
#include "../../include/sexpr/SExpr.hpp"
#include "../../include/sexpr/SymAtom.hpp"
#include "../sexpr/cast.cpp"
#include <algorithm>
#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

using std::all_of;
using std::cerr;
using std::cin;
using std::cout;
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

shared_ptr<SExpr> parseAtom(string token) {
  if (all_of(token.begin(), token.end(), ::isdigit) ||
      (token[0] == '-' && token.length() > 1 &&
       all_of(token.begin() + 1, token.end(), ::isdigit))) {
    return make_shared<IntAtom>(stoi(token));
  }
  return make_shared<SymAtom>(token);
}

shared_ptr<SExpr> parse(vector<string>::iterator &it) {
  string token = *it;
  it += 1;
  if (token == ")") {
    return make_shared<NilAtom>();
  } else if (token == "(") {
    shared_ptr<SExpr> first = parse(it);
    return make_shared<SExprs>(first, parse(it));
  }
  return make_shared<SExprs>(parseAtom(token), parse(it));
}

shared_ptr<SExpr> parse(vector<string> tokens) {
  if (tokens.size() == 1) {
    return parseAtom(tokens.front());
  }
  auto it = tokens.begin() + 1;
  return parse(it);
}

shared_ptr<SExpr> eval(shared_ptr<SExpr> sExpr, shared_ptr<Env> env) {
  try {
    if (isa<NilAtom>(*sExpr) || isa<IntAtom>(*sExpr) || isa<BoolAtom>(*sExpr)) {
      return sExpr;
    } else if (isa<SymAtom>(*sExpr)) {
      return env->find(cast<SymAtom>(sExpr)->val);
    }
    shared_ptr<SExprs> sExprs = cast<SExprs>(sExpr);
    if (isa<SymAtom>(*sExprs->first)) {
      shared_ptr<SymAtom> sym = cast<SymAtom>(sExprs->first);
      if (sym->val == "define") {
        sExprs = cast<SExprs>(sExprs->rest);
        string name = cast<SymAtom>(sExprs->first)->val;
        shared_ptr<SExpr> val = eval(cast<SExprs>(sExprs->rest)->first, env);
        env->symTable.insert(make_pair(name, val));
        return val;
      } else if (sym->val == "set!") {
        sExprs = cast<SExprs>(sExprs->rest);
        string name = cast<SymAtom>(sExprs->first)->val;
        shared_ptr<SExpr> val = eval(cast<SExprs>(sExprs->rest)->first, env);
        env->set(name, val);
        return val;
      } else if (sym->val == "quote") {
        sExprs = cast<SExprs>(sExprs->rest);
        return sExprs->first;
      } else if (sym->val == "if") {
        sExprs = cast<SExprs>(sExprs->rest);
        shared_ptr<BoolAtom> test =
            std::make_shared<BoolAtom>(eval(sExprs->first, env));
        shared_ptr<SExpr> conseq = cast<SExprs>(sExprs->rest)->first;
        shared_ptr<SExpr> alt =
            cast<SExprs>(cast<SExprs>(sExprs->rest)->rest)->first;
        return (test->val) ? eval(conseq, env) : eval(alt, env);
      } else if (sym->val == "lambda") {
        sExprs = cast<SExprs>(sExprs->rest);
        shared_ptr<SExpr> argNames = sExprs->first;
        shared_ptr<SExpr> body = cast<SExprs>(sExprs->rest)->first;

        return make_shared<ClosureAtom>(
            [body](shared_ptr<Env> env) { return eval(body, env); }, env,
            argNames);
      }
    }
    shared_ptr<ClosureAtom> closure =
        cast<ClosureAtom>(eval(sExprs->first, env));
    return (*closure)(sExprs->rest, env);
  } catch (EvalException ee) {
    ee.pushStackTrace(sExpr);
    throw ee;
  }
}

void repl() {
  shared_ptr<Env> env = make_shared<Env>();
  initEnv(env);

  while (true) {
    cout << "lisp> ";
    string input;
    getline(cin, input);
    if (!all_of(input.begin(), input.end(), isspace)) {
      try {
        cout << *eval(parse(tokenize(input)), env) << endl;
      } catch (EvalException ee) {
        cerr << "Error: " << ee.what() << endl << endl;
        cerr << "Eval stack:" << endl;
        for (auto it : ee.getStackTrace()) {
          cerr << "=> " << *it << endl;
        }
        cerr << endl;
      }
    }
  }
}
