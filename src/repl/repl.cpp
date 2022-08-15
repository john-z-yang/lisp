#include "../repl/repl.hpp"
#include "../env/Env.hpp"
#include "../env/functions.hpp"
#include "../eval/EvalException.hpp"
#include "../eval/eval.hpp"
#include "../sexpr/Atom.hpp"
#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/ClosureAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SymAtom.hpp"
#include "../sexpr/cast.cpp"
#include "ParseException.hpp"
#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <regex>
#include <string>
#include <vector>

using std::all_of;
using std::cerr;
using std::cin;
using std::cout;
using std::distance;
using std::endl;
using std::getline;
using std::istream;
using std::make_shared;
using std::regex;
using std::regex_replace;
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

void handleUnexpectedChar(const string name, const string line,
                          const string::size_type pos) {
  stringstream ss;
  ss << "Unexpected \"" << name << "\".";
  throw ParseException(ss.str(), line, pos);
}

istream &getInput(istream &in, ostream &out, string &str, string prompt,
                  string wrap) {
  str.erase();
  int openParen = 0;
  int closedParen = 0;
  string line;
  out << prompt;
  while (getline(in, line)) {
    replace_if(line.begin(), line.end(), ::isspace, ' ');
    line = regex_replace(line, regex("^ +| +$|( ) +"), "$1");
    if (str.empty() && line.empty()) {
      out << prompt;
      continue;
    }
    for (auto it = line.begin(); it != line.end(); ++it) {
      if (openParen == closedParen && openParen > 0) {
        handleUnexpectedChar(string(1, *it), line, distance(line.begin(), it));
      }
      if (*it == '(') {
        openParen += 1;
      } else if (*it == ')') {
        closedParen += 1;
      }
    }
    str += line + " ";
    if (openParen == closedParen) {
      if (openParen > 0 || line.find(' ') == string::npos) {
        return in;
      }
      handleUnexpectedChar(string(1, ' '), line, line.find(' '));
    }
    out << wrap;
  }
  return in;
}

int repl() {
  shared_ptr<Env> env = make_shared<Env>();
  initEnv(env);
  while (true) {
    try {
      string input;
      if (getInput(cin, cout, input, "lisp> ", "  ... ")) {
        cout << *eval(parse(tokenize(input)), env) << endl;
      } else {
        cout << endl;
        lispQuit(nullptr);
      }
    } catch (ParseException pe) {
      cerr << pe;
    } catch (EvalException ee) {
      cerr << ee;
    }
  }
  return EXIT_SUCCESS;
}
