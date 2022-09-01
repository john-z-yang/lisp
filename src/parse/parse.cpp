#include "../sexpr/BoolAtom.hpp"
#include "../sexpr/IntAtom.hpp"
#include "../sexpr/NilAtom.hpp"
#include "../sexpr/SExpr.hpp"
#include "../sexpr/SExprs.hpp"
#include "../sexpr/SymAtom.hpp"
#include "ParseException.hpp"
#include <algorithm>
#include <iterator>
#include <memory>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

using std::all_of;
using std::back_inserter;
using std::copy;
using std::cout;
using std::distance;
using std::getline;
using std::istream;
using std::make_shared;
using std::regex;
using std::regex_replace;
using std::replace_if;
using std::shared_ptr;
using std::sregex_token_iterator;
using std::stoi;
using std::string;
using std::stringstream;
using std::vector;

vector<string> tokenize(string str) {
  vector<string> tokens;
  regex rgx("\\(|\\)|,@|,|`|'|[^\\s(),@,`']+");
  copy(sregex_token_iterator(str.begin(), str.end(), rgx, 0),
       sregex_token_iterator(), back_inserter(tokens));
  return tokens;
}

shared_ptr<SExpr> parseAtom(string token) {
  if ((token.length() >= 1 && all_of(token.begin(), token.end(), ::isdigit)) ||
      (token[0] == '-' && token.length() > 1 &&
       all_of(token.begin() + 1, token.end(), ::isdigit))) {
    return make_shared<IntAtom>(stoi(token));
  }
  if (token == "#t") {
    return make_shared<BoolAtom>(true);
  }
  if (token == "#f") {
    return make_shared<BoolAtom>(false);
  }
  if (token == "'") {
    return make_shared<SymAtom>("quote");
  }
  if (token == "`") {
    return make_shared<SymAtom>("quasiquote");
  }
  if (token == ",") {
    return make_shared<SymAtom>("unquote");
  }
  if (token == ",@") {
    return make_shared<SymAtom>("unquote-splicing");
  }
  return make_shared<SymAtom>(token);
}

shared_ptr<SExpr> parse(vector<string>::iterator &it);

shared_ptr<SExpr> parseSexprs(vector<string>::iterator &it) {
  string token = *it;
  if (token == ")") {
    it += 1;
    return make_shared<NilAtom>();
  } else if (token == "(") {
    it += 1;
    shared_ptr<SExpr> first = parseSexprs(it);
    shared_ptr<SExpr> rest = parseSexprs(it);
    return make_shared<SExprs>(first, rest);
  }
  shared_ptr<SExpr> first = parse(it);
  shared_ptr<SExpr> rest = parseSexprs(it);
  return make_shared<SExprs>(first, rest);
}

shared_ptr<SExpr> parse(vector<string>::iterator &it) {
  string token = *it;
  it += 1;
  if (token == "(") {
    return parseSexprs(it);
  }
  if (token == "'" || token == "`" || token == "," || token == ",@") {
    shared_ptr<SExpr> rest =
        make_shared<SExprs>(parse(it), make_shared<NilAtom>());
    return make_shared<SExprs>(parseAtom(token), rest);
  }
  return parseAtom(token);
}

shared_ptr<SExpr> parse(string str) {
  vector<string> tokens = tokenize(str);
  vector<string>::iterator it = tokens.begin();
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
    line = regex_replace(line, regex("^\\s+|\\s+$|(\\s)\\s+|;.*"), "$1");
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
