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
using std::fstream;
using std::getline;
using std::istream;
using std::make_shared;
using std::regex;
using std::regex_replace;
using std::shared_ptr;
using std::stoi;
using std::string;
using std::vector;

vector<string> tokenize(string str) {
  vector<string> tokens;
  string token;
  for (const char c : str) {
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
                          const string::size_type charPos) {
  stringstream ss;
  ss << "Unexpected \"" << name << "\".";
  throw ParseException(ss.str(), line, charPos);
}

void verifyLex(std::string &line, int &openParen, int &closedParen) {
  for (auto it = line.begin(); it != line.end(); ++it) {
    if ((openParen == closedParen && openParen > 0 && !isspace(*it)) ||
        (openParen == closedParen && *it == ')')) {
      handleUnexpectedChar(string(1, *it), line, distance(line.begin(), it));
    }
    if (*it == '(') {
      openParen += 1;
    } else if (*it == ')') {
      closedParen += 1;
    }
  }
  string::size_type wsPos = line.find(' ');
  if (openParen == 0 && wsPos != string::npos) {
    handleUnexpectedChar(string(1, line[wsPos + 1]), line, wsPos + 1);
  }
}

istream &getInput(istream &in, string &str, size_t &linesRead, string prompt,
                  string wrap) {
  int openParen = 0;
  int closedParen = 0;
  string line;
  cout << prompt;
  while (getline(in, line)) {
    linesRead += 1;
    replace_if(line.begin(), line.end(), ::isspace, ' ');
    line = regex_replace(line, regex("^ +| +$|( ) +"), "$1");
    if (str.empty() && line.empty()) {
      cout << prompt;
      continue;
    }
    verifyLex(line, openParen, closedParen);
    str += line + " ";
    if (openParen == closedParen) {
      return in;
    }
    cout << wrap;
  }
  return in;
}

int repl() {
  shared_ptr<Env> env = make_shared<Env>();
  initEnv(env);
  while (true) {
    string input;
    size_t linesRead = 0;
    try {
      if (getInput(cin, input, linesRead, "lisp> ", "  ... ")) {
        cout << *eval(parse(tokenize(input)), env) << endl;
      } else {
        cout << endl;
        lispQuit(nullptr);
      }
    } catch (ParseException pe) {
      cerr << "In line " << linesRead << " of <std::cin>" << endl;
      cerr << pe;
    } catch (EvalException ee) {
      cerr << "In line " << linesRead << " of <std::cin>" << endl;
      cerr << ee;
    }
  }
  return EXIT_FAILURE;
}

int repl(const string fileName) {
  fstream fs;
  fs.open(fileName, fstream::in);

  if (fs.fail()) {
    cerr << "Unable to open file \"" << fileName << "\": " << strerror(errno)
         << endl;
    return EXIT_FAILURE;
  }

  shared_ptr<Env> env = make_shared<Env>();
  initEnv(env);

  size_t linesRead = 0;
  while (true) {
    string input;
    try {
      if (getInput(fs, input, linesRead, "", "")) {
        *eval(parse(tokenize(input)), env);
      } else {
        break;
      }
    } catch (ParseException pe) {
      cerr << "In line " << linesRead << " of \"" << fileName << "\"" << endl;
      cerr << pe;
      return EXIT_FAILURE;
    } catch (EvalException ee) {
      cerr << "In line " << linesRead << " of \"" << fileName << "\"" << endl;
      cerr << ee;
      return EXIT_FAILURE;
    }
  }
  return EXIT_SUCCESS;
}
