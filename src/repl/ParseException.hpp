#ifndef LISP_SRC_REPL_PARSEEXCEPTION_HPP_
#define LISP_SRC_REPL_PARSEEXCEPTION_HPP_

#include <exception>
#include <ostream>
#include <string>

using std::exception;
using std::ostream;
using std::string;

class ParseException : public exception {
  string _msg;

public:
  const string line;
  const string::size_type pos;

  ParseException(const string &msg, const string line,
                 const string::size_type pos);

  virtual const char *what() const noexcept override;
};

ostream &operator<<(ostream &o, const ParseException &pe);

#endif