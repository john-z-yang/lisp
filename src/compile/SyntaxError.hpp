#ifndef LISP_SRC_COMPILE_SYNTAXERROR_HPP_
#define LISP_SRC_COMPILE_SYNTAXERROR_HPP_

#include <exception>
#include <ostream>
#include <string>

class SyntaxError : public std::exception {
public:
  const std::string line;
  const std::string::size_type charPos;

  SyntaxError(const std::string &msg, const std::string line,
              const std::string::size_type charPos);

  virtual const char *what() const noexcept override;

private:
  std::string _msg;
};

std::ostream &operator<<(std::ostream &o, const SyntaxError &pe);

#endif
