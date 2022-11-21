#ifndef LISP_SRC_PARSE_PARSEEXCEPTION_HPP_
#define LISP_SRC_PARSE_PARSEEXCEPTION_HPP_

#include <exception>
#include <ostream>
#include <string>

class ParseException : public std::exception {
  std::string _msg;

public:
  const std::string line;
  const std::string::size_type charPos;

  ParseException(const std::string &msg, const std::string line,
                 const std::string::size_type charPos);

  virtual const char *what() const noexcept override;
};

std::ostream &operator<<(std::ostream &o, const ParseException &pe);

#endif